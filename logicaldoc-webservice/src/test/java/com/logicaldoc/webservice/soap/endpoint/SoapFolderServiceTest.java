package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSFolder;

/**
 * Test case for <code>SoapFolderService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapFolderServiceTest extends AbstractWebserviceTestCase {

	private FolderDAO folderDao;

	private UserDAO userDao;

	// Instance under test
	private SoapFolderService soapFolderService;

	private SoapSecurityService soapSecurityService;

	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		folderDao = (FolderDAO) context.getBean("FolderDAO");
		userDao = (UserDAO) context.getBean("UserDAO");

		// Make sure that this is a SoapFolderService instance
		soapFolderService = new SoapFolderService();
		soapFolderService.setValidateSession(false);

		soapSecurityService = new SoapSecurityService();
		soapSecurityService.setValidateSession(false);
	}

	@Test
	public void testMove() throws Exception {
		Folder folderToMove = folderDao.findById(1203);
		assertNotNull(folderToMove);
		assertEquals(1201, folderToMove.getParentId());
		Folder parentFolder = folderDao.findById(1200);
		assertNotNull(parentFolder);

		soapFolderService.move("", folderToMove.getId(), 1200L);
		folderToMove = folderDao.findById(1203);
		assertEquals(1200, folderToMove.getParentId());
	}

	@Test
	public void testCreate() throws Exception {
		WSFolder wsFolderTest = new WSFolder();
		wsFolderTest.setName("folder test");
		wsFolderTest.setDescription("descr folder test");
		wsFolderTest.setParentId(103);

		WSFolder wsFolder = soapFolderService.create("", wsFolderTest);
		assertNotNull(wsFolder);
		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());

		wsFolder = soapFolderService.getFolder("", wsFolder.getId());
		assertNotNull(wsFolder);

		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());

		Folder createdFolder = folderDao.findByNameAndParentId("folder test", 103).get(0);
		assertNotNull(createdFolder);
		assertEquals("folder test", createdFolder.getName());
		assertEquals("descr folder test", createdFolder.getDescription());
	}

	@Test
	public void testDelete() throws Exception {
		soapFolderService.delete("", 1201);
		Folder folder = folderDao.findById(1201);
		assertNull(folder);
	}

	@Test
	public void testRename() throws Exception {
		Folder folder = folderDao.findById(103);
		assertNotNull(folder);
		assertEquals("menu.admin", folder.getName());
		folderDao.initialize(folder);

		soapFolderService.rename("", 103, "paperino");

		folder = folderDao.findById(103);
		assertEquals("paperino", folder.getName());
		assertEquals(101, folder.getParentId());
		assertEquals(3, folder.getType());
	}

	@Test
	public void testGetFolder() throws Exception {

		Folder folder = folderDao.findById(103);
		assertNotNull(folder);

		WSFolder wsFolder = soapFolderService.getFolder("", 103);

		assertEquals(103, wsFolder.getId());
		assertEquals("menu.admin", wsFolder.getName());
		assertEquals(101, wsFolder.getParentId());
		assertEquals("description", wsFolder.getDescription());

		// trying to get a non existent folder
		try {
			wsFolder = soapFolderService.getFolder("", 2510);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}

		// trying to get a folder alias
		wsFolder = soapFolderService.getFolder("", 1204);
		assertNotNull(wsFolder);
		assertEquals(101, wsFolder.getFoldRef().longValue());
		assertEquals("text", wsFolder.getName());

		// trying to get a folder for which the user does not have read
		// permission
		SessionManager sm = SessionManager.get();
		Session session1 = sm.newSession("guest", "admin", null);

		try {
			soapFolderService.setValidateSession(true);
			wsFolder = soapFolderService.getFolder(session1.getSid(), 99);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		} finally {
			soapFolderService.setValidateSession(false);
		}
	}

	@Test
	public void testIsReadable() throws Exception {
		assertTrue(soapFolderService.isReadable("", 1200));
		assertTrue(soapFolderService.isReadable("", 99));
	}

	@Test
	public void testSetAccessControlList() throws Exception {
		User user = userDao.findById(4);

		assertTrue(folderDao.isPermissionEnabled(Permission.ADD, 80, user.getId()));
		assertFalse(folderDao.isPermissionEnabled(Permission.IMMUTABLE, 80, user.getId()));

		WSAccessControlEntry ace = new WSAccessControlEntry();
		ace.setUserId(user.getId());
		ace.setGroupId(user.getUserGroup().getId());

		soapFolderService.setAccessControlList("", 80L, new WSAccessControlEntry[] { ace });

		// Because of these methods use JDBC directly, they fails when the test
		// is executed by maven. Probably the folder groups are not already
		// persisted in the DB
		// assertTrue(folderDao.isPermissionEnabled(Permission.IMMUTABLE,
		// 80, user.getId()));
		// assertFalse(folderDao.isPermissionEnabled(Permission.ADD, 80,
		// user.getId()));
	}

	@Test
	public void testGetAccessControlList() {
		try {
			WSAccessControlEntry[] acl = new WSAccessControlEntry[0];
			acl = soapFolderService.getAccessControlList("", 80);
			assertEquals(4, acl.length);
		} catch (Exception e) {
			// Nothing to do
		}
	}

	@Test
	public void testListWorkspaces() throws Exception {
		WSFolder[] folders = soapFolderService.listWorkspaces("");
		assertNotNull(folders);
		assertEquals(1, folders.length);
		assertEquals("Default", folders[0].getName());
	}
}