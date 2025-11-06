package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttribute;
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

	private SoapFolderService testSubject;

	private SoapSecurityService soapSecurityService;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		folderDao = FolderDAO.get();
		userDao = UserDAO.get();

		// Make sure that this is a SoapFolderService instance
		testSubject = new SoapFolderService();
		testSubject.setValidateSession(false);

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

		testSubject.move("", folderToMove.getId(), 1200L);
		folderToMove = folderDao.findById(1203);
		assertEquals(1200, folderToMove.getParentId());
	}

	@Test
	public void testCopy() throws Exception {
		assertEquals(2, testSubject.listChildren("", 1200L).size());
		assertTrue(testSubject.listChildren("", 1200L).stream().noneMatch(f -> f.getName().equals("menu.adminxxx")));

		testSubject.copy("", 80L, 1200L, 0, "inherit");

		assertEquals(3, testSubject.listChildren("", 1200L).size());
		assertTrue(testSubject.listChildren("", 1200L).stream().anyMatch(f -> f.getName().equals("menu.adminxxx")));

		testSubject.copy("", 99L, 1200L, 1, "replicate");
		assertEquals(4, testSubject.listChildren("", 1200L).size());
		assertTrue(testSubject.listChildren("", 1200L).stream().anyMatch(f -> f.getName().equals("menu.admin")));
	}

	@Test
	public void testCreate() throws Exception {
		WSFolder wsFolderTest = new WSFolder();
		wsFolderTest.setName("folder test");
		wsFolderTest.setDescription("descr folder test");
		wsFolderTest.setParentId(103);

		WSFolder wsFolder = testSubject.create("", wsFolderTest);
		assertNotNull(wsFolder);
		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());

		wsFolder = testSubject.getFolder("", wsFolder.getId());
		assertNotNull(wsFolder);

		assertEquals("folder test", wsFolder.getName());
		assertEquals(103, wsFolder.getParentId());

		Folder createdFolder = folderDao.findByNameAndParentId("folder test", 103).get(0);
		assertNotNull(createdFolder);
		assertEquals("folder test", createdFolder.getName());
		assertEquals("descr folder test", createdFolder.getDescription());
	}

	@Test
	public void testCreateAlias() throws Exception {
		WSFolder alias = testSubject.createAlias("", 1201L, 101L);
		assertEquals(101L, alias.getFoldRef().longValue());
	}

	@Test
	public void testCreateFolder() throws Exception {
		long folderId = testSubject.createFolder("", 1201L, "test");
		WSFolder folder = testSubject.getFolder("", folderId);

		assertEquals(1201L, folder.getParentId());
		assertEquals("test", folder.getName());
	}

	@Test
	public void testFindByPath() throws Exception {
		WSFolder folder = testSubject.findByPath("", "/menu.adminxxx/text");
		assertEquals(101L, folder.getId());
	}

	@Test
	public void testDelete() throws Exception {
		testSubject.delete("", 1201);
		Folder folder = folderDao.findById(1201);
		assertNull(folder);
	}

	@Test
	public void testRename() throws Exception {
		Folder folder = folderDao.findById(103);
		assertNotNull(folder);
		assertEquals("menu.admin103", folder.getName());
		folderDao.initialize(folder);

		testSubject.rename("", 103, "paperino");

		folder = folderDao.findById(103);
		assertEquals("paperino", folder.getName());
		assertEquals(101, folder.getParentId());
		assertEquals(3, folder.getType());
	}

	@Test
	public void testGetFolder() throws Exception {

		Folder folder = folderDao.findById(103);
		assertNotNull(folder);

		WSFolder wsFolder = testSubject.getFolder("", 103);

		assertEquals(103, wsFolder.getId());
		assertEquals("menu.admin103", wsFolder.getName());
		assertEquals(101, wsFolder.getParentId());
		assertEquals("description", wsFolder.getDescription());

		// trying to get a non existent folder
		assertNull(testSubject.getFolder("", 2510));
		

		// trying to get a folder alias
		wsFolder = testSubject.getFolder("", 1204);
		assertNotNull(wsFolder);
		assertEquals(101, wsFolder.getFoldRef().longValue());
		assertEquals("text", wsFolder.getName());

		// trying to get a folder for which the user does not have read
		// permission
		SessionManager sm = SessionManager.get();
		Session session1 = sm.newSession("guest", "admin", (Client) null);

		try {
			testSubject.setValidateSession(true);
			testSubject.getFolder(session1.getSid(), 99);
			fail("Expected exception was not thrown");
		} catch (Exception e) {
			// nothing to do here
		} finally {
			testSubject.setValidateSession(false);
		}
	}

	@Test
	public void testIsReadable() throws Exception {
		assertTrue(testSubject.isReadable("", 1200));
		assertTrue(testSubject.isReadable("", 99));
	}

	@Test
	public void testList() throws Exception {
		List<WSFolder> folders = testSubject.listChildren("", 5L);
		assertEquals(4, folders.size());
		assertTrue(folders.stream().anyMatch(f -> f.getName().equals("menu.admin")));
	}

	@Test
	public void testSetAccessControlList() throws Exception {
		User user = userDao.findById(4);

		assertTrue(folderDao.isPermissionAllowed(Permission.ADD, 80, user.getId()));
		assertFalse(folderDao.isPermissionAllowed(Permission.IMMUTABLE, 80, user.getId()));

		WSAccessControlEntry ace = new WSAccessControlEntry();
		ace.setUserId(user.getId());
		ace.setGroupId(user.getUserGroup().getId());

		testSubject.setAccessControlList("", 80L, List.of(ace));
	}

	@Test
	public void testGetAccessControlList() {
		try {
			List<WSAccessControlEntry> acl = testSubject.getAccessControlList("", 80);
			assertEquals(4, acl.size());
		} catch (Exception e) {
			// Nothing to do
		}
	}

	@Test
	public void testListWorkspaces() throws Exception {
		List<WSFolder> folders = testSubject.listWorkspaces("");
		assertNotNull(folders);
		assertEquals(1, folders.size());
		assertEquals("Default", folders.get(0).getName());
	}

	@Test
	public void testGetPath() throws Exception {
		List<WSFolder> path = testSubject.getPath("", 103);
		assertNotNull(path);
		assertEquals(4, path.size());
	}

	@Test
	public void testGetDefault() throws Exception {
		WSFolder folder = testSubject.getDefaultWorkspace("");
		assertEquals(Folder.DEFAULTWORKSPACEID, folder.getId());

		folder = testSubject.getRootFolder("");
		assertEquals(Folder.ROOTID, folder.getId());
	}

	@Test
	public void testIsWritable() throws Exception {
		assertTrue(testSubject.isWritable("", 1200L));
		assertTrue(testSubject.isGranted("", 1200L, Permission.DOWNLOAD.name()));
	}

	@Test
	public void testCreatePath() throws Exception {
		WSFolder folder = testSubject.createPath("", Folder.DEFAULTWORKSPACEID, "/pippo/pluto/paperino");
		assertEquals("paperino", folder.getName());

		WSFolder folder2 = testSubject.findByPath("", "/Default/pippo/pluto/paperino");
		assertEquals(folder.getId(), folder2.getId());
		
		folder = testSubject.createPath("", Folder.DEFAULTWORKSPACEID, "pippo/pluto/paperino/archimede");
		assertEquals("archimede", folder.getName());
		
		folder = testSubject.createPath("", Folder.ROOTID, "/abc/def");
		assertEquals("def", folder.getName());
		
		folder = testSubject.createPath("", Folder.ROOTID, "/Default/ghi/lmn");
		assertEquals("lmn", folder.getName());
	}
	
	@Test
	public void testUpdate() throws Exception {
		WSFolder folder = testSubject.getFolder("", 1201L);
		assertEquals("abc", folder.getName());
		assertTrue(folder.getAttributes().isEmpty());

		folder.setName("newname");
		folder.setTemplateId(-1L);
		WSAttribute att=new WSAttribute();
		att.setName("source");
		att.setStringValue("val1");
		folder.addAttribute(att);
		
		testSubject.update("", folder);
		
		folder = testSubject.getFolder("", 1201L);
		assertEquals("newname", folder.getName());
		assertEquals("val1", folder.getAttributes().get(0).getStringValue());
	}
	
	
	@Test
	public void testMerge() throws Exception {
		assertEquals(2, testSubject.listChildren("", 101L).size());
		assertEquals(2, testSubject.listChildren("", 1200L).size());
		testSubject.merge("", 101L, 1200L);
		assertNull(testSubject.getFolder("", 101L));
		assertEquals(3, testSubject.listChildren("", 1200L).size());
	}
}