package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.List;

import javax.activation.DataHandler;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.Test;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstFolderRecursiveDownload extends BaseTestCase {

	protected static Log log = LogFactory.getLog(TstFolderRecursiveDownload.class);

	private SoapFolderClient folderClient;

	private SoapDocumentClient documentClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		folderClient = new SoapFolderClient(settings.getProperty("url") + "/services/Folder");
		documentClient = new SoapDocumentClient(settings.getProperty("url") + "/services/Document");
	}

	@Test
	public void testFolderRecursiveDownload() throws Exception {

		// This is the ID of the initial folder to explore
		long folderID = 25165829L;
		// $folderID = 5; // start from the default workspace
		String basePath = "C:/tmp";

		listFoldersRecursive(folderID, basePath);
	}

	
	private void listFoldersRecursive(long parentFolderID, String basePath) throws Exception {
		WSFolder fcreated = folderClient.getFolder(sid, parentFolderID);

		// Create path for the current folder
		String parentPath = basePath + "/" + fcreated.getName();
		// check if the dir exists on the filesystem

		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}

		// Download documents in the current folder to disk
		downloadDocumentsInFolder(parentFolderID, parentPath);

		// Get folder children
		List<WSFolder> children = folderClient.listChildren(sid, parentFolderID);

		// Cycle over the results and download all the documents to their
		// folders
		for (WSFolder child : children) {
			System.out.println("folderId: " + child.getId());
			System.out.println("Name: " + child.getName());
			System.out.println("----------------");
			listFoldersRecursive(child.getId(), parentPath);
		}
	}

	private void downloadDocumentsInFolder(long parentFolderID, String parentPath) throws Exception {

		// Download all the documents in Default Workspace folder (id=4,
		// name='Default')
		long folderId = parentFolderID;
		List<WSDocument> docs = documentClient.listDocuments(sid, folderId, null);

		for (WSDocument wsDocument : docs) {
			long docId = wsDocument.getId();
			System.out.println(docId + " : " + wsDocument.getFileName());

			DataHandler dh = documentClient.getContent(sid, docId);

			File outFile = new File(parentPath, wsDocument.getFileName());
			try (OutputStream os = new FileOutputStream(outFile)) {
				IOUtils.copy(dh.getInputStream(), os);
			}
		}
	}
}
