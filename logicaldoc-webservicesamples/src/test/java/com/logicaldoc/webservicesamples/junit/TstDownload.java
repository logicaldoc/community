package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.activation.DataHandler;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class TstDownload extends BaseUnit  {

	protected static Log log = LogFactory.getLog(TstDownload.class);

	public TstDownload(String arg0) {
		super(arg0);
	}	

	public void testDownloadDocument() throws IOException {

		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		OutputStream os = null;

		try {
			// Download the document with id 3932160
			// You should be sure that there is a document on LD with this ID
			long docId = 24543232L;

			WSDocument mydoc = docc.getDocument(sid, docId);

			System.out.println(mydoc.getFileName());

			File destFile = new File("C:/tmp", mydoc.getFileName());
			os = new FileOutputStream(destFile);

			DataHandler dh = docc.getContent(sid, docId);
			IOUtils.copy(dh.getInputStream(), os);
		} catch (RuntimeException e) {
			log.error("Unable to download the document: " + e.getMessage(), e);
		} catch (Exception e) {
			e.printStackTrace();
		}

		os.close();
	}

	public void testDownloadFolderContent() throws Exception {

		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		// Download all the documents in Default Workspace folder (id=4, name='Default')
		long folderId = DEFAULT_WORKSPACE;

		WSDocument[] docs = docc.listDocuments(sid, folderId, null);
		
		if (docs != null) {
			log.error("docs.length: " + docs.length);
			for (WSDocument wsDocument : docs) {
				long docId = wsDocument.getId();
				System.out.println(docId +" : " + wsDocument.getFileName());

				DataHandler dh = docc.getContent(sid, docId);

				File outFile = new File("C:/tmp", wsDocument.getFileName());
				OutputStream os = new FileOutputStream(outFile);
				IOUtils.copy(dh.getInputStream(), os);
				os.close();
			}
		} else {
			log.error("No documents found in Root folder");
		}
	}

}
