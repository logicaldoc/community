package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.FileDataSource;

import org.apache.commons.io.FilenameUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstMassiveImportSack extends BaseTestCase {

	private static final int TIME_ATTENTION_TRESHOLD = 30000;

	private static final int MAX_DOCUMENTS_TOBE_INSERTED = 15000;

	private static final Logger log = LoggerFactory.getLogger(TstMassiveImportSack.class);

	private long startTime;

	public int docsInserted = 0;

	public int foldersCreated = 0;

	private SoapFolderClient folderClient;

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		folderClient = new SoapFolderClient(settings.getProperty("url") + "/services/Folder");
		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();

		log.error("Time: " + new java.util.Date());
		log.error("Inserted Documents: " + docsInserted);
		log.error("Created Folders: " + foldersCreated);
		long elapsedTime = System.currentTimeMillis() - startTime;
		double seconds = elapsedTime / 1000;
		log.error("Job ended in: " + Math.round(seconds) + " seconds");
		log.error("Job started at: " + new java.util.Date(startTime));
		log.error("Job ended at: " + new java.util.Date());
	}

	@Test
	public void testMassiveImportSack() {
		docsInserted = 0;
		startTime = System.currentTimeMillis();
		try {
			// String sDir = "C:/Users/Cecio/Documents/Logical"; // 1866
			// documents, 270 folders
			String sDir = "C:/Users/alle/Documents/Logical"; // 9220 files,
																// 1096
																// folders
			importFolderSack(sDir, DEFAULT_WORKSPACE);
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("Documents currently Created: " + docsInserted);
		}
	}

	private void importFolderSack(String sDir, long parentId) throws AuthenticationException, PermissionException,
			PersistenceException, WebserviceException, IOException {

		File[] faFiles = new File(sDir).listFiles();

		for (File file : faFiles) {
			if (docsInserted >= MAX_DOCUMENTS_TOBE_INSERTED) {
				System.err.println("Reached limit of " + MAX_DOCUMENTS_TOBE_INSERTED + " document imported");
				return;
			}

			if (file.isDirectory()) {
				// System.out.println("FOLDER: " + file.getName());
				// Creation of the folder tree
				long childFolder = createFolder(file.getName(), parentId);
				importFolderSack(file.getAbsolutePath(), childFolder);
			} else {
				// if (file.length() < 31615) {
				// }
				if (docsInserted % 100 == 0)
					System.err.println("Documents currently Created: " + docsInserted);

				// ****************************************************
				// Add file to document repository
				// ****************************************************
				System.out.println(file.getAbsolutePath());
				// Import document
				long templateid = 2L;

				createDocumentSack(parentId, file, templateid, getAttributes(templateid));
				docsInserted++;
			}
		}
	}

	private List<WSAttribute> getAttributes(long templateid) {

		List<WSAttribute> attributes = new ArrayList<WSAttribute>();
		Date deldate = new Date();
		String fPattern2 = new String("yyyy-MM-dd");
		SimpleDateFormat dateFmtyyyyMMdd = new SimpleDateFormat(fPattern2);

		if (templateid == 2) {
			WSAttribute attr1 = new WSAttribute();
			attr1.setName("docid");
			attr1.setType(WSAttribute.TYPE_STRING);
			attr1.setStringValue("09XM0050093");
			attributes.add(attr1);

			WSAttribute attr2 = new WSAttribute();
			attr2.setName("deldate");
			attr2.setType(WSAttribute.TYPE_DATE);
			attr2.setDateValue(dateFmtyyyyMMdd.format(deldate));
			attributes.add(attr2);

			WSAttribute attr3 = new WSAttribute();
			attr3.setName("delroute");
			attr3.setType(WSAttribute.TYPE_STRING);
			attr3.setStringValue("005");
			attributes.add(attr3);

			WSAttribute attr4 = new WSAttribute();
			attr4.setName("outlet");
			attr4.setType(WSAttribute.TYPE_INT);
			attr4.setIntValue(Long.valueOf(966838L));
			attributes.add(attr4);

			WSAttribute attr5 = new WSAttribute();
			attr5.setName("salesrep");
			attr5.setType(WSAttribute.TYPE_STRING);
			attr5.setStringValue("D288");
			attributes.add(attr5);

			WSAttribute attr6 = new WSAttribute();
			attr6.setName("driver");
			attr6.setType(WSAttribute.TYPE_STRING);
			attr6.setStringValue("D288");
			attributes.add(attr6);
		}

		return attributes;
	}

	private void createDocumentSack(long targetFolder, File file, long templateid, List<WSAttribute> attributes)
			throws AuthenticationException, PermissionException, PersistenceException, IOException,
			WebserviceException {

		// DataSource ds = new FileDataSource(file);
		DataSource ds = new FileDataSource(new File("C:/tmp/DEL_01X06180519_07-01-2011.pdf"));
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		// document.setFileName(file.getName());
		String baseName = FilenameUtils.getBaseName(file.getName());
		document.setFileName(baseName + ".pdf");

		document.setFolderId(targetFolder);
		document.setLanguage("en");

		if (attributes != null && attributes.size() > 0) {
			document.setTemplateId(templateid);
			document.setAttributes(attributes);
		}

		long startTime = System.currentTimeMillis();
		WSDocument docRes = documentClient.create(sid, document, content);
		System.out.println("Created documentID = " + docRes.getId());
		long timeElapsed = System.currentTimeMillis() - startTime;
		if (timeElapsed > TIME_ATTENTION_TRESHOLD)
			System.err.println("Document created in: " + timeElapsed + " ms");

	}

	private long createFolder(String name, long parentId)
			throws AuthenticationException, PermissionException, PersistenceException, WebserviceException {

		// Check if a subfolder of the parent folder has the same name
		List<WSFolder> children = folderClient.listChildren(sid, parentId);
		for (WSFolder wsFolder : children) {
			if (wsFolder.getName().equals(name)) {
				return wsFolder.getId();
			}
		}

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName(name);
		folder.setParentId(parentId);

		WSFolder createdFolder = folderClient.create(sid, folder);
		foldersCreated++;
		System.out.println("Created folderID = " + createdFolder.getId());
		return createdFolder.getId();
	}
}