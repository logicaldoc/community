package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstRecursiveImport extends BaseTestCase {

	protected static Log log = LogFactory.getLog(TstRecursiveImport.class);

	private static String[] extensionArray = new String[] { "pdf", "txt", "html", "gif", "jpg", "xls", "xlsx" };

	List<String> allowedExtensions = Arrays.asList(extensionArray);

	private SoapFolderClient folderClient;

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		folderClient = new SoapFolderClient(settings.getProperty("url") + "/services/Folder");
		documentClient = new SoapDocumentClient(
				settings.getProperty("url") + "/services/Document");
	}

	@Test
	public void testRecursiveFolderImport() throws AuthenticationException, PermissionException, PersistenceException,
			IOException, WebserviceException {
		String sDir = "C:/Users/alle/Desktop/LinkDetox";
		importTree(sDir, DEFAULT_WORKSPACE);
	}

	private void importTree(String sDir, long parentId) throws AuthenticationException, PermissionException,
			PersistenceException, IOException, WebserviceException {

		File[] faFiles = new File(sDir).listFiles();
		for (File file : faFiles) {
			if (file.isDirectory()) {
				// System.out.println("FOLDER: " + file.getName());
				// Creation of the folder tree
				long childFolder = createFolder(file.getName(), parentId);
				importTree(file.getAbsolutePath(), childFolder);
			} else {
				System.out.println(file.getAbsolutePath());
				// System.out.println(file.getName());
				// Import document
				importDocument(file, parentId);
			}
		}
	}

	private void importDocument(File file, long parentId) throws AuthenticationException, PermissionException,
			PersistenceException, IOException, WebserviceException {

		String fileExt = FilenameUtils.getExtension(file.getName());
		if (!allowedExtensions.contains(fileExt.toLowerCase()))
			return;

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setFolderId(parentId);
		document.setFileName(file.getName());

		WSDocument docRes = documentClient.create(sid, document, content);

		System.out.println("documentID = " + docRes.getId());
	}

	private long createFolder(String name, long parentId)
			throws AuthenticationException, PermissionException, PersistenceException, WebserviceException {

		// Check if a subfolder of the parent folder has the same name
		List<WSFolder> children = folderClient.listChildren(sid, parentId);
		for (WSFolder wsFolder : children) {
			if (wsFolder.getName().equals(name)) {
				System.out.println("FOLDER EXIST");
				return wsFolder.getId();
			}
		}

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName(name);
		folder.setParentId(parentId);
		WSFolder fcreated = folderClient.create(sid, folder);
		return fcreated.getId();
	}
}