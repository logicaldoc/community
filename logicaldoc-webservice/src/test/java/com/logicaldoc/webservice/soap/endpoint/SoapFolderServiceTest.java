package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderGroup;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;

/**
 * Test case for <code>SoapFolderService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapFolderServiceTest extends AbstractWebserviceTestCase {

	private FolderDAO folderDao;

	private UserDAO userDao;

	private GroupDAO groupDao;

	// Instance under test
	private SoapFolderService folderServiceImpl;

	private SoapSecurityService securityServiceImpl;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		folderDao = (FolderDAO) context.getBean("FolderDAO");
		userDao = (UserDAO) context.getBean("UserDAO");
		groupDao = (GroupDAO) context.getBean("GroupDAO");

		// Make sure that this is a SoapFolderService instance
		folderServiceImpl = new SoapFolderService();
		folderServiceImpl.setValidateSession(false);
		
		securityServiceImpl = new SoapSecurityService();
		securityServiceImpl.setValidateSession(false);
	}

	@Test
	public void testMove() throws Exception {
		Folder folderToMove = folderDao.findById(1203);
		assertNotNull(folderToMove);
		assertEquals(1201, folderToMove.getParentId());
		Folder parentFolder = folderDao.findById(1200);
		assertNotNull(parentFolder);

		folderServiceImpl.move("", folderToMove.getId(), 1200L);
		folderToMove = folderDao.findById(1203);
		assertEquals(1200, folderToMove.getParentId());
	}

	@Test
	public void testCreate() throws Exception {
		WSFolder wsFolderTest = new WSFolder();
		wsFolderTest.setName("folder test");
		wsFolderTest.setDescription("descr folder test");
		wsFolderTest.setParentId(103);

		WSFolder wsFolder = folderServiceImpl.create("", wsFolderTest);
		assertNotNull(wsFolder);
		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());

		wsFolder = folderServiceImpl.getFolder("", wsFolder.getId());
		assertNotNull(wsFolder);
		System.out.println(">>>>>> "+wsFolder.getId() +"    "+wsFolder.getName()+"    "+wsFolder.getDescription());
		
		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());
		
		Folder createdFolder = folderDao.findByNameAndParentId("folder test", 103).get(0);
		assertNotNull(createdFolder);
		assertEquals("folder test", createdFolder.getName());
		assertEquals("descr folder test", createdFolder.getDescription());
	}

	@Test
	public void testDelete() throws Exception {
		folderServiceImpl.delete("", 1201);
		Folder folder = folderDao.findById(1201);
		assertNull(folder);
	}

	@Test
	public void testRename() throws Exception {
		Folder folder = folderDao.findById(103);
		assertNotNull(folder);
		assertEquals("menu.admin", folder.getName());
		folderDao.initialize(folder);

		folderServiceImpl.rename("", 103, "paperino");

		folder = folderDao.findById(103);
		assertEquals("paperino", folder.getName());
		assertEquals(101, folder.getParentId());
		assertEquals(3, folder.getType());
	}

	@Test
	public void testGetFolder() throws Exception {
		
		Folder folder = folderDao.findById(103);
		assertNotNull(folder);

		WSFolder wsFolder = folderServiceImpl.getFolder("", 103);

		assertEquals(103, wsFolder.getId());
		assertEquals("menu.admin", wsFolder.getName());
		assertEquals(101, wsFolder.getParentId());
		assertEquals("description", wsFolder.getDescription());
		
		// trying to get a non existent folder
		try {
			wsFolder = folderServiceImpl.getFolder("", 2510);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		}
		
		// trying to get a folder alias
		wsFolder = folderServiceImpl.getFolder("", 1204);
		assertNotNull(wsFolder);
		assertEquals(101, wsFolder.getFoldRef().longValue());
		assertEquals("text", wsFolder.getName());
		
		// trying to get a folder for which the user does not have read permission
		SessionManager sm = SessionManager.get();
		Session session1 = sm.newSession("guest", "admin", null);
		
		try {
			folderServiceImpl.setValidateSession(true);		
			wsFolder = folderServiceImpl.getFolder(session1.getSid(), 99);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		} finally {		
			folderServiceImpl.setValidateSession(false);
		}
	}

	@Test
	public void testIsReadable() throws Exception {
		assertTrue(folderServiceImpl.isReadable("", 1200));
		assertTrue(folderServiceImpl.isReadable("", 99));
	}

	@Test
	public void testGrantUser() throws Exception {
		User user = userDao.findById(4);

		assertTrue(folderDao.isPermissionEnabled(Permission.ADD, 80, user.getId()));
		assertFalse(folderDao.isPermissionEnabled(Permission.IMMUTABLE, 80, user.getId()));

		int permissionsInt = 4091;
		assertFalse(Permission.ADD.match(permissionsInt));
		assertTrue(Permission.IMMUTABLE.match(permissionsInt));
		folderServiceImpl.grantUser("", 80, user.getId(), permissionsInt, false);

		// Because of these methods use JDBC directly, they fails when the test
		// is executed by maven. Probably the folder groups are not already
		// persisted in the DB
		// assertTrue(folderDao.isPermissionEnabled(Permission.IMMUTABLE,
		// 80, user.getId()));
		// assertFalse(folderDao.isPermissionEnabled(Permission.ADD, 80,
		// user.getId()));
	}

	@Test
	public void testGrantGroup() throws Exception {
		Group group = groupDao.findById(3);
		assertNotNull(group);
		Folder folder = folderDao.findById(99);
		assertNotNull(folder);
		Folder folder2 = folderDao.findById(80);
		assertNotNull(folder2);
		folderDao.initialize(folder);
		FolderGroup mg = folder.getFolderGroup(3);
		assertNull(mg);
		folderDao.initialize(folder2);
		FolderGroup mg2 = folder2.getFolderGroup(3);
		assertNotNull(mg2);

		folderServiceImpl.grantGroup("", 99, 3, 4095, false);

//		folder = folderDao.findById(99);
//		folderDao.initialize(folder);
//		mg = folder.getFolderGroup(3);
//		assertNotNull(mg);
	}

	@Test
	public void testGetGrantedUsers() {
		try {
			WSRight[] rights = new WSRight[0];
			rights = folderServiceImpl.getGrantedUsers("", 80);
			assertEquals(2, rights.length);
			assertEquals(3, rights[0].getId());
		} catch (Exception e) {
			// Nothing to do
		}
	}

	@Test
	public void testGetGrantedGroups() throws Exception {
		WSRight[] rights = new WSRight[0];
		rights = folderServiceImpl.getGrantedGroups("", 80);
		assertEquals(2, rights.length);
	}

	@Test
	public void testListWorkspaces() throws Exception {
		WSFolder[] folders = folderServiceImpl.listWorkspaces("");
		assertNotNull(folders);
		assertEquals(1, folders.length);
		assertEquals("Default", folders[0].getName());
	}
}