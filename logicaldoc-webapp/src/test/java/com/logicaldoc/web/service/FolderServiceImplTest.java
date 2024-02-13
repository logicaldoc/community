package com.logicaldoc.web.service;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.web.AbstractWebappTestCase;

import junit.framework.Assert;

public class FolderServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private FolderServiceImpl service = new FolderServiceImpl();

	private FolderDAO folderDao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		folderDao = (FolderDAO) context.getBean("FolderDAO");
	}

	@Test
	public void testSave() throws ServerException {
		GUIFolder folder = service.getFolder(6, false, false, false);

		folder = service.save(folder);
		Assert.assertNotNull(folder);
		Assert.assertEquals("folder6", folder.getName());
		Assert.assertEquals(5, folder.getParentId());

		folder = service.getFolder(1200, false, false, false);

		folder = service.save(folder);
		Assert.assertNotNull(folder);
		Assert.assertEquals("test", folder.getName());
		Assert.assertEquals(5, folder.getParentId());
	}

	@Test
	public void testRename() throws ServerException, PersistenceException {
		Folder folder = folderDao.findById(6);
		Assert.assertEquals("folder6", folder.getName());

		service.rename(6, "pluto");

		folder = folderDao.findById(6);
		Assert.assertEquals("pluto", folder.getName());
	}

	@Test
	public void testApplyRights() throws ServerException, PersistenceException {
		Folder parentFolder = folderDao.findById(6);
		Assert.assertNotNull(parentFolder);
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.DELETE, 1201, 3));
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.RENAME, 1201, 3));
		Folder childFolder1 = folderDao.findById(1202);
		Assert.assertNotNull(childFolder1);
		Assert.assertEquals(1201, childFolder1.getParentId());
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.DELETE, 1202, 3));
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.RENAME, 1202, 3));

		GUIFolder folder = service.getFolder(6, false, false, false);

		service.saveACL(folder, true);

		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.DELETE, 1202, 1));
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.RENAME, 1202, 1));
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.DELETE, 1201, 1));
		Assert.assertTrue(folderDao.isPermissionEnabled(Permission.RENAME, 1201, 1));
	}

	@Test
	public void testGetFolder() throws ServerException {
		GUIFolder folder = service.getFolder(6, false, false, false);
		Assert.assertNotNull(folder);
		Assert.assertEquals("folder6", folder.getName());
		Assert.assertEquals(5, folder.getParentId());

		folder = service.getFolder(1202, true, false, false);
		Assert.assertNotNull(folder);
		Assert.assertEquals("xyz", folder.getName());
		Assert.assertEquals(1201, folder.getParentId());
		Assert.assertEquals("/test/ABC/xyz", folder.getPathExtended());

		// Try with unexisting id
		folder = null;
		try {
			folder = service.getFolder(9999, false, false, false);
		} catch (ServerException e) {
			// We expect an exception here;
		}
		Assert.assertNull(folder);
	}

	@Test
	public void testMoveFolder_Simple() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderA = folderDao.create(docsFolder, new Folder("folderA"), true, null);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);

		service.move(List.of(folderC.getId()), folderA.getId());

		List<Folder> folderList = folderDao.findChildren(folderA.getId(), null);
		Assert.assertEquals(1, folderList.size());

		Assert.assertTrue(folderList.contains(folderC));
	}

	@Test
	public void testMoveFolder_Up() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderA = folderDao.create(docsFolder, new Folder("folderA"), true, null);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);
		folderDao.create(folderC, new Folder("folderD"), true, null);
		folderDao.create(folderC, new Folder("folderE"), true, null);

		service.move(List.of(folderC.getId()), folderA.getId());

		List<Folder> folderList = folderDao.findChildren(folderA.getId(), null);
		Assert.assertEquals(1, folderList.size());
		Assert.assertTrue(folderList.contains(folderC));

		folderList = folderDao.findChildren(folderB.getId(), null);
		Assert.assertEquals(0, folderList.size());
	}

	@Test
	public void testMoveFolder_Down() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);
		Folder folderD = folderDao.create(folderC, new Folder("folderD"), true, null);
		Folder folderE = folderDao.create(folderC, new Folder("folderE"), true, null);
		folderDao.create(folderE, new Folder("folderF"), true, null);

		service.move(List.of(folderE.getId()), folderD.getId());

		List<Folder> folderList = folderDao.findChildren(folderD.getId(), null);
		Assert.assertEquals(1, folderList.size());
		Assert.assertTrue(folderList.contains(folderE));

		folderList = folderDao.findChildren(folderC.getId(), null);
		Assert.assertEquals(1, folderList.size());
	}
}