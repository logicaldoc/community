package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.util.List;

import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.activation.FileDataSource;

import org.apache.commons.io.FilenameUtils;
import org.junit.Test;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstDocumentCRUD extends BaseTestCase {

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Test
	public void testDocumentCRUD() throws Exception {
		File file = new File("C:/tmp/Get-started-EN.pdf");

		DataSource ds = new FileDataSource(file);
		DataHandler content = new DataHandler(ds);

		WSDocument document = new WSDocument();
		document.setLanguage("en");
		document.setFolderId(DEFAULT_WORKSPACE);
		document.setFileName(file.getName());

		WSDocument docRes = documentClient.create(sid, document, content);

		System.err.println("documentID = " + docRes.getId());

		// Retrieve the metadata of the document created
		long docId = docRes.getId();
		WSDocument docInfo = documentClient.getDocument(sid, docId);

		System.out.println(docInfo.getFileName());

		// Update the metadata of the document created
		// in particular we decided to update fields: title and tags
		try {
			docInfo.setFileName("MyTitle." + FilenameUtils.getExtension(file.getName()));
			docInfo.setTags(List.of("tag", "keyword", "test"));
			documentClient.update(sid, docInfo);
		} catch (RuntimeException e1) {
			e1.printStackTrace();
		}

		// VERIFY THE CHANGES to the tags
		WSDocument docInfoUpd = documentClient.getDocument(sid, docId);
		List<String> tags2 = docInfoUpd.getTags();
		assertNotNull(tags2);
		assertEquals(3, tags2.size());

		// Delete the document just created
		documentClient.delete(sid, docId);

		// verify the effective deletion of the document
		try {
			docInfo = documentClient.getDocument(sid, docId);
			assertNull(docInfo);
		} catch (RuntimeException e) {
			e.printStackTrace();
		}
	}
}