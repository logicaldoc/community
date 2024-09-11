package com.logicaldoc.webservicesamples;

import java.io.File;
import java.io.IOException;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.rest.client.RestAuthClient;
import com.logicaldoc.webservice.rest.client.RestDocumentClient;

public class RestWorkbench {

	private static ContextProperties settings;

	static {
		try {
			settings = new ContextProperties();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void checkin() throws Exception {
		RestDocumentClient documentClient = instantiateDocumentClient();

		long docID = 100L;
		documentClient.checkout(docID);
		System.out.println("Checkout performed");

		File file = new File("C:/tmp/Taylor Swift - I Knew You Were Trouble (1080P 24Fps Av1-128Kbit Aac)-1b.m4v");

		// get the filesize in order to check
		long fsize = file.length();
		System.out.println("FileSize of the new file version: " + fsize);

		documentClient.checkin(docID, "rest checkin", false, file);
		System.out.println("Operation completed");

		// Checks the filesize after the check-in operation
		WSDocument doc = documentClient.getDocument(docID);
		System.out.println("FileSize after checkin: " + doc.getFileSize());
	}

	private static RestDocumentClient instantiateDocumentClient() {
		RestDocumentClient rclient = new RestDocumentClient(settings.getProperty("url")+ "/services/rest/document", settings.getProperty("apiKey"));
		return rclient;
	}

	public static void main(String[] args) throws Exception {
		createDocument();
		checkin();
		logout();
	}

	private static void createDocument() throws Exception {
		RestDocumentClient documentClient = instantiateDocumentClient();

		File file = new File("C:/tmp/1989 Style - 720Suq-1.m4v");
		if (!file.exists()) {
			throw new Exception("upload File not found!");
		}

		// get the filesize in order to check
		long fsize = file.length();
		System.out.println("FileSize of the source file: " + fsize);

		long folderID = 4L; // Default workspace folder

		WSDocument wsdocument = new WSDocument();
		wsdocument.setFolderId(folderID);
		wsdocument.setFileName(file.getName());

		WSDocument createdDoc = documentClient.create(wsdocument, file);
		System.out.println("Operation completed");
		System.out.println("Document created with ID: " + createdDoc.getId());

		// Checks the filesize after the upload operation [OPTIONAL]
		System.out.println("FileSize of the created doc: " + createdDoc.getFileSize());
	}

	public static void logout() throws Exception {
		RestAuthClient auth = new RestAuthClient(settings.getProperty("url")+ "/services/rest/auth");
		auth.logout(auth.getSid());
	}

}
