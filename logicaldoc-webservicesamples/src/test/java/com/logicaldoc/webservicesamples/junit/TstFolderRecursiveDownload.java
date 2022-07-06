package com.logicaldoc.webservicesamples.junit;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import javax.activation.DataHandler;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstFolderRecursiveDownload extends BaseUnit  {

	protected static Log log = LogFactory.getLog(TstFolderRecursiveDownload.class);

	public TstFolderRecursiveDownload(String arg0) {
		super(arg0);
	}	
	
	public void testFolderRecursiveDownload() throws Exception {
		
		// This is the ID of the initial folder to explore
		long folderID = 25165829L;
	  //$folderID = 5; // start from the default workspace
		String basePath = "C:/tmp";
		
		listFoldersRecursive(folderID, basePath);
	}
	
	public void listFoldersRecursive(long parentFolderID, String basePath) throws Exception {
		
		SoapFolderClient folderc = new SoapFolderClient(FOLDER_ENDPOINT);
		
		WSFolder fcreated = folderc.getFolder(sid, parentFolderID);
		
		// Create path for the current folder
		String parentPath = basePath + "/" + fcreated.getName();
		//check if the dir exists on the filesystem
		
		File file = new File(parentPath);
		if (!file.exists()) {
			file.mkdirs();
		}
		
		// Download documents in the current folder to disk	
		downloadDocumentsInFolder(parentFolderID, parentPath);
		
		// Get folder children 
		WSFolder[] fchild = folderc.listChildren(sid, parentFolderID);
		
		// Cycle over the results and download all the documents to their folders
		if (fchild != null && fchild.length > 0) {
			for (int i = 0; i < fchild.length; i++) {
				System.out.println("folderId: " + fchild[i].getId());
				System.out.println("Name: " + fchild[i].getName());
				System.out.println("----------------");
				listFoldersRecursive (fchild[i].getId(), parentPath);
			}
		}		
	}

	private void downloadDocumentsInFolder(long parentFolderID, String parentPath) throws Exception {
		
		SoapDocumentClient docc = new SoapDocumentClient(DOC_ENDPOINT);

		// Download all the documents in Default Workspace folder (id=4, name='Default')
		long folderId = parentFolderID;
		WSDocument[] docs = docc.listDocuments(sid, folderId, null);
		if (docs != null) {
			log.error("docs.length: " + docs.length);
			for (WSDocument wsDocument : docs) {
				long docId = wsDocument.getId();
				System.out.println(docId +" : " + wsDocument.getFileName());

				DataHandler dh = docc.getContent(sid, docId);

				File outFile = new File(parentPath, wsDocument.getFileName());
				OutputStream os = new FileOutputStream(outFile);
				IOUtils.copy(dh.getInputStream(), os);
				os.close();
			}
		} else {
			log.info("No documents found in folder: " +folderId);
		}
		
	}


}
