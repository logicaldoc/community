package com.logicaldoc.webservice.soap.endpoint;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderGroup;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.webservice.AbstractWebServiceTestCase;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;

/**
 * Test case for <code>SoapFolderService</code>
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.2
 */
public class SoapFolderServiceTest extends AbstractWebServiceTestCase {

	private FolderDAO folderDao;

	private UserDAO userDao;

	private GroupDAO groupDao;

	// Instance under test
	private SoapFolderService folderServiceImpl;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		folderDao = (FolderDAO) context.getBean("FolderDAO");
		userDao = (UserDAO) context.getBean("UserDAO");
		groupDao = (GroupDAO) context.getBean("GroupDAO");

		// Make sure that this is a SoapFolderService instance
		folderServiceImpl = new SoapFolderService();
		folderServiceImpl.setValidateSession(false);
	}

	@Test
	public void testMove() throws Exception {
		Folder folderToMove = folderDao.findById(1203);
		Assert.assertNotNull(folderToMove);
		Assert.assertEquals(1201, folderToMove.getParentId());
		Folder parentFolder = folderDao.findById(1200);
		Assert.assertNotNull(parentFolder);

		folderServiceImpl.move("", folderToMove.getId(), 1200L);
		folderToMove = folderDao.findById(1203);
		Assert.assertEquals(1200, folderToMove.getParentId());
	}

	@Test
	public void testCreate() throws Exception {
		WSFolder wsFolderTest = new WSFolder();
		wsFolderTest.setName("folder test");
		wsFolderTest.setDescription("descr folder test");
		wsFolderTest.setParentId(103);

		WSFolder wsFolder = folderServiceImpl.create("", wsFolderTest);
		Assert.assertNotNull(wsFolder);
		Assert.assertEquals("folder test", wsFolder.getName());
		Assert.assertEquals(103, wsFolder.getParentId());

		Folder createdFolder = folderDao.findByNameAndParentId("folder test", 103).get(0);
		Assert.assertNotNull(createdFolder);
		Assert.assertEquals("folder test", createdFolder.getName());
		Assert.assertEquals("descr folder test", createdFolder.getDescription());
	}

	@Test
	public void testDelete() throws Exception {
		folderServiceImpl.delete("", 1201);
		Folder folder = folderDao.findById(1201);
		Assert.assertNull(folder);
	}

	@Test
	public void testRename() throws Exception {
		Folder folder = folderDao.findById(103);
		Assert.assertNotNull(folder);
		Assert.assertEquals("menu.admin", folder.getName());
		folderDao.initialize(folder);

		folderServiceImpl.rename("", 103, "paperino");

		folder = folderDao.findById(103);
		Assert.assertEquals("paperino", folder.getName());
		Assert.assertEquals(101, folder.getParentId());
		Assert.assertEquals(3, folder.getType());
	}

	@Test
	public void testGetFolder() throws Exception {
		Folder folder = folderDao.findById(103);
		Assert.assertNotNull(folder);
		
		WSFolder wsFolder = folderServiceImpl.getFolder("", 103);

		Assert.assertEquals(103, wsFolder.getId());
		Assert.assertEquals("menu.admin", wsFolder.getName());
		Assert.assertEquals(101, wsFolder.getParentId());
		Assert.assertEquals("description", wsFolder.getDescription());
	}

	@Test
	public void testIsReadable() throws Exception {
		Assert.assertTrue(folderServiceImpl.isReadable("", 1200));
		Assert.assertTrue(folderServiceImpl.isReadable("", 99));
	}

	@Test
	public void testGrantUser() throws Exception {
		User user = userDao.findById(4);

		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.ADD, 100, user.getId()));
		Assert.assertFalse(folderDao.isPermissionEnabled(Permission.IMMUTABLE, 100, user.getId()));

		folderServiceImpl.grantUser("", 100, user.getId(), 4091, false);

		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.IMMUTABLE, 100, user.getId()));
		Assert.assertFalse(folderDao.isPermissionEnabled(Permission.ADD, 100, user.getId()));
	}

	@Test
	public void testGrantGroup() throws Exception {
		Group group = groupDao.findById(3);
		Assert.assertNotNull(group);
		Folder folder = folderDao.findById(99);
		Assert.assertNotNull(folder);
		Folder folder2 = folderDao.findById(100);
		Assert.assertNotNull(folder2);
		FolderGroup mg = folder.getFolderGroup(3);
		Assert.assertNull(mg);
		FolderGroup mg2 = folder2.getFolderGroup(3);
		Assert.assertNotNull(mg2);

		folderServiceImpl.grantGroup("", 99, 3, 4095, false);

		folder = folderDao.findById(99);
		mg = folder.getFolderGroup(3);
		Assert.assertNotNull(mg);
	}

	@Test
	public void testGetGrantedUsers() {
		try {
			WSRight[] rights = new WSRight[0];
			rights = folderServiceImpl.getGrantedUsers("", 100);
			Assert.assertEquals(2, rights.length);
			Assert.assertEquals(3, rights[0].getId());
		} catch (Exception e) {
		}
	}

	@Test
	public void testGetGrantedGroups() throws Exception {
		WSRight[] rights = new WSRight[0];
		rights = folderServiceImpl.getGrantedGroups("", 100);
		Assert.assertEquals(2, rights.length);
	}

	@Test
	public void testListWorkspaces() throws Exception {
		WSFolder[] folders = folderServiceImpl.listWorkspaces("");
		Assert.assertNotNull(folders);
		Assert.assertEquals(1, folders.length);
		Assert.assertEquals("Default", folders[0].getName());
	}
}