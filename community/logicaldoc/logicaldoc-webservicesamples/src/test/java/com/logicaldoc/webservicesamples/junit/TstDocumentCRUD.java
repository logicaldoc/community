package com.logicaldoc.webservicesamples.junit;

import java.io.File;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstDocumentCRUD extends BaseUnit {

	protected static Log log = LogFactory.getLog(TstDocumentCRUD.class);

	public TstDocumentCRUD(String arg0) {
		super(arg0);
	}

	public void testDocumentCRUD() throws Exception {

		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		File file = new File("C:/tmp/Get-started-EN.pdf");

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setLanguage("en");
		document.setFolderId(DEFAULT_WORKSPACE);
		document.setFileName(file.getName());

		WSDocument docRes = docc.create(sid, document, content);

		System.err.println("documentID = " + docRes.getId());

		// Retrieve the metadata of the document created
		long docId = docRes.getId();
		WSDocument docInfo = docc.getDocument(sid, docId);

		System.out.println(docInfo.getFileName());

		// Update the metadata of the document created
		// in particular we decided to update fields: title and tags
		try {
			String[] tags = new String[] { "tag", "keyword", "test" };

			docInfo.setFileName("MyTitle." + FilenameUtils.getExtension(file.getName()));
			docInfo.setTags(tags);
			docc.update(sid, docInfo);
		} catch (RuntimeException e1) {
			e1.printStackTrace();
		}

		// VERIFY THE CHANGES to the tags
		WSDocument docInfoUpd = docc.getDocument(sid, docId);
		String[] tags2 = docInfoUpd.getTags();
		assertNotNull(tags2);
		assertEquals(3, tags2.length);

		// Delete the document just created
		docc.delete(sid, docId);

		// verify the effective deletion of the document
		try {
			docInfo = docc.getDocument(sid, docId);
			assertNull(docInfo);
		} catch (RuntimeException e) {
			e.printStackTrace();
		}
	}
}