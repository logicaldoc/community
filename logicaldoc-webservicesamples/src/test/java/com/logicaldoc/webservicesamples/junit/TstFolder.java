package com.logicaldoc.webservicesamples.junit;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstFolder extends BaseUnit  {

	protected static Log log = LogFactory.getLog(TstFolder.class);

	public TstFolder(String arg0) {
		super(arg0);
	}
	
	public void testFolderPath() throws IOException {

		SoapFolderClient folderc = new SoapFolderClient(FOLDER_ENDPOINT);

		// The parent folder is default workspace folder: 4L
		long parentFolderId = DEFAULT_WORKSPACE;
		try {
			// Folder Creation
			// result is the newly created folder or exception			
			String path = "2012/004/16/doctype";
			WSFolder fcreated = folderc.createPath(sid, parentFolderId, path);
			
			System.out.println(fcreated.getId());
			System.out.println(fcreated.getName());
			
			assertEquals("doctype", fcreated.getName());
			
			// Get the path of a folder
			WSFolder[] sss = folderc.getPath(sid, fcreated.getId());
			for (int i = 0; i < sss.length; i++) {
				System.out.println(sss[i].getName());
			}
			
		} catch (Exception e) {
			e.printStackTrace();
			fail("Exception during one of the CRD operation");
		}	
	}

	public void testFolderCRUD() throws IOException {

		SoapFolderClient folderc = new SoapFolderClient(FOLDER_ENDPOINT);

		// The parent folder is default workspace folder: 4L
		long parentFolderId = DEFAULT_WORKSPACE;
		try {
			// Folder Creation
			// result is the string "error" or the newly created folderId
			WSFolder folder = new WSFolder();
			folder.setName("myFirstFolder");
			folder.setParentId(parentFolderId);
			WSFolder fcreated = folderc.create(sid, folder);
			
			System.out.println(fcreated.getId());
			log.error("fcreated.getId(): " +fcreated.getId());

			// Folder Retrieval
			long createdFolder = fcreated.getId();
			WSFolder childFolder = folderc.getFolder(sid, createdFolder);
			
			System.out.println(childFolder.getName());
			log.error("childFolder.getName(): " +childFolder.getName());
			assertEquals("myFirstFolder", childFolder.getName());
			
			// Folder Rename
			folderc.rename(sid, createdFolder, "New Folder Name");
			
			WSFolder verFolder = folderc.getFolder(sid, createdFolder);
			System.out.println(verFolder.getName());
			log.error("verFolder.getName(): " +verFolder.getName());

			// Folder Delete
			folderc.delete(sid, createdFolder);

			// verify the folder deletion
			try {
				WSFolder delFolder = folderc.getFolder(sid, createdFolder);
				fail("folder: " + delFolder + " should not exist");
			} catch (Exception e) {
			}

		} catch (Exception e) {
			e.printStackTrace();
			fail("Exception during one of the CRUD operation");
		}
	}

}
