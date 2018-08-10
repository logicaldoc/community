package com.logicaldoc.webservicesamples;
import java.io.File;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.rest.client.RestAuthClient;
import com.logicaldoc.webservice.rest.client.RestDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;

public class RestWorkbench {

	/*
	public static void mainCheckin(String[] args) throws Exception {

		/**
		 * Cross-check the file size before and after the checkin
		 * Use the new REST web-service for the check in the file.
		 * The special feature of the REST API (LD 7.0.x) check-in method is to allow the upload of large files
		 
		String base = "http://localhost:8080/services/rest";
		String soapBase = "http://localhost:8080/services";
		
		String username = "admin";
		String password = "admin";		
		
		RestDocumentClient doc = new RestDocumentClient(base + "/document", username, password);
		SoapDocumentClient soapclient = new SoapDocumentClient(soapBase + "/Document");

		try {
			long docID = 16416768L;
			doc.checkout(sid, docID);
			System.out.println("Checkout performed");
			
			File file = new File("C:/tmp/logicaldoc-dropbox.m2v");
			// get the filesize in order to check
			long fsize = file.length();
			System.out.println("FileSize of the new file version: " +fsize);						
			
			doc.checkin(sid, docID, "rest checkin", file.getName(), false, file);
			System.out.println("Operation completed");
			
			// Checks the filesize after the check-in operation (using the SOAP ws)
			WSDocument xxx = soapclient.getDocument(sid, docID);
			System.out.println("FileSize after checkin: " + xxx.getFileSize());
		} finally {
			auth.logout();
		}		
	}
	*/
	
	public static void main(String[] args) throws Exception {

		/**
		 * Performs the creation of a new document using the REST API (LD 7.0.x).
		 * The special feature of the REST API upload method allows the upload of large files
		 */
		
		String base = "http://localhost:8080/services/rest";
		
		String username = "admin";
		String password = "admin";

		RestDocumentClient restClient = new RestDocumentClient(base + "/document", username, password);

			File file = new File("C:/tmp/logicaldoc-dropbox.m2v");
			if (!file.exists()) {
				throw new Exception("upload File not found!");
			}
			
			// get the filesize in order to check
			long fsize = file.length();
			System.out.println("FileSize of the source file: " +fsize);								

			long folderID = 4L; // Default workspace folder
			
			WSDocument wsdocument = new WSDocument();
			wsdocument.setFolderId(folderID);
			wsdocument.setFileName(file.getName());
			
			WSDocument createdDoc = restClient.create(wsdocument, file);
										
			System.out.println("Operation completed");
			System.out.println("Document created with ID: " + createdDoc.getId());
			
			// Checks the filesize after the upload operation [OPTIONAL]
			System.out.println("FileSize of the created doc: " +createdDoc.getFileSize());
	}
	

	/*
	public static void mainAuth(String[] args) throws Exception {
		
		/**
		 * Performs authentication using the REST api
	
		String base = "http://localhost:8080/services/rest";
		RestAuthClient auth = new RestAuthClient(base + "/auth");

		// Open a session
		String sid = auth.login("admin", "admin");
		System.out.println("Created session: " + sid);
		if (auth.valid(sid))
			System.out.println("Session is valid");

		auth.renew(sid);
		System.out.println("Session renewed");

		try {
		} finally {
			auth.logout();
		}
	}
	*/
}
