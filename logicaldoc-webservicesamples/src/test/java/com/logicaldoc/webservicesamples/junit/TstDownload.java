package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import jakarta.activation.DataHandler;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstDownload extends BaseTestCase {

	private static final Logger log = LoggerFactory.getLogger(TstDownload.class);

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Test
	public void testDownloadDocument() throws IOException {

		OutputStream os = null;
		try {
			// Download the document with id 3932160
			// You should be sure that there is a document on LD with this ID
			long docId = 24543232L;

			WSDocument mydoc = documentClient.getDocument(sid, docId);

			System.out.println(mydoc.getFileName());

			File destFile = new File("C:/tmp", mydoc.getFileName());
			os = new FileOutputStream(destFile);

			DataHandler dh = documentClient.getContent(sid, docId);
			IOUtils.copy(dh.getInputStream(), os);
		} catch (RuntimeException e) {
			log.error("Unable to download the document: " + e.getMessage(), e);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			IOUtils.close(os);
		}
	}

	@Test
	public void testDownloadFolderContent() throws Exception {
		// Download all the documents in Default Workspace folder (id=4,
		// name='Default')
		long folderId = DEFAULT_WORKSPACE;

		List<WSDocument> docs = documentClient.listDocuments(sid, folderId, null);
		for (WSDocument wsDocument : docs) {
			long docId = wsDocument.getId();
			System.out.println(docId + " : " + wsDocument.getFileName());

			DataHandler dh = documentClient.getContent(sid, docId);

			File outFile = new File("C:/tmp", wsDocument.getFileName());
			try (OutputStream os = new FileOutputStream(outFile)) {
				IOUtils.copy(dh.getInputStream(), os);
			}
		}
	}
}