package com.logicaldoc.webservicesamples.junit;

import java.io.IOException;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;

public class TstFolder extends BaseTestCase {

	private static final Logger log = LoggerFactory.getLogger(TstFolder.class);

	private SoapFolderClient folderClient;

	@Override
	protected void setUp() throws Exception {
		super.setUp();

		folderClient = new SoapFolderClient(settings.getProperty("url") + "/services/Folder");
	}

	@Test
	public void testFolderPath() throws IOException, AuthenticationException, PermissionException, PersistenceException,
			WebserviceException {

		// The parent folder is default workspace folder: 4L
		long parentFolderId = DEFAULT_WORKSPACE;
		// Folder Creation
		// result is the newly created folder or exception
		String path = "2012/004/16/doctype";
		WSFolder createdFolder = folderClient.createPath(sid, parentFolderId, path);
		System.out.println(createdFolder.getId());
		System.out.println(createdFolder.getName());

		assertEquals("doctype", createdFolder.getName());

		// Get the path of a folder
		List<WSFolder> folders = folderClient.getPath(sid, createdFolder.getId());
		for (WSFolder wsFolder : folders) {
			System.out.println(wsFolder.getName());
		}
	}

	@Test
	public void testFolderCRUD() throws IOException, AuthenticationException, PermissionException, PersistenceException,
			WebserviceException {
		// The parent folder is default workspace folder: 4L
		long parentFolderId = DEFAULT_WORKSPACE;

		// Folder Creation
		// result is the string "error" or the newly created folderId
		WSFolder folder = new WSFolder();
		folder.setName("myFirstFolder");
		folder.setParentId(parentFolderId);
		WSFolder createdFolder = folderClient.create(sid, folder);

		System.out.println(createdFolder.getId());
		log.error("createdFolder.getId(): " + createdFolder.getId());

		// Folder Retrieval
		long createdFolderId = createdFolder.getId();
		WSFolder childFolder = folderClient.getFolder(sid, createdFolderId);

		System.out.println(childFolder.getName());
		log.error("childFolder.getName(): " + childFolder.getName());
		assertEquals("myFirstFolder", childFolder.getName());

		// Folder Rename
		folderClient.rename(sid, createdFolderId, "New Folder Name");

		WSFolder verFolder = folderClient.getFolder(sid, createdFolderId);
		System.out.println(verFolder.getName());
		log.error("verFolder.getName(): " + verFolder.getName());

		// Folder Delete
		folderClient.delete(sid, createdFolderId);

		// verify the folder deletion
		WSFolder delFolder = folderClient.getFolder(sid, createdFolderId);
		fail("folder: " + delFolder + " should not exist");
	}
}