package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.AbstractWPTestCase;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.data.FoldersDataServlet;

public class FolderServiceImplTest extends AbstractWPTestCase {

	// Instance under test
	private FolderServiceImpl testSubject = new FolderServiceImpl();

	private FolderDAO folderDao;

	private DocumentDAO documentDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		folderDao = Context.get(FolderDAO.class);
		documentDao = Context.get(DocumentDAO.class);
	}

	@Test
	public void testSave() throws ServerException {
		GUIFolder folder = testSubject.getFolder(6, true, true, true);

		folder = testSubject.save(folder);
		assertNotNull(folder);
		assertEquals("folder6", folder.getName());
		assertEquals(5, folder.getParentId());

		folder.setName("XYZ");
		folder = testSubject.save(folder);
		assertNotNull(folder);

		folder = testSubject.getFolder(folder.getId(), true, true, true);
		assertNotNull(folder);
		assertEquals("XYZ", folder.getName());

		folder = testSubject.getFolder(1200, true, true, true);

		folder = testSubject.save(folder);
		assertNotNull(folder);
		assertEquals("test", folder.getName());
		assertEquals(5, folder.getParentId());

		folder = testSubject.getFolder(6L, true, true, true);
		folder.setFoldRef(9L);
		folder = testSubject.save(folder);
		assertNotNull(folder);
		folder = testSubject.getFolder(6, true, true, true);
		assertEquals("XYZ", folder.getName());

		// Remove any template assignment
		folder.setTemplate(null);
		folder.setTemplateId(null);
		folder.getAttributes().clear();
		folder = testSubject.save(folder);

		folder.setTemplateId(-1L);
		GUIAttribute attribute = new GUIAttribute();
		attribute.setName("source");
		attribute.setValue("test");
		folder.getAttributes().add(attribute);

		attribute = new GUIAttribute();
		attribute.setName("object");
		attribute.setValue(1);
		folder.getAttributes().add(attribute);

		folder.addTag("pippo");

		folder = testSubject.save(folder);
		assertNotNull(folder);
		assertNotNull(folder.getTemplateId());
		assertEquals(9, folder.getAttributes().size());
	}

	@Test
	public void testMerge() throws ServerException, PersistenceException {
		assertTrue(folderDao.findChildren(6L, null).isEmpty());
		testSubject.merge(List.of(7L, 1200L), 6L);

		assertNotNull(folderDao.findByNameAndParentId("folder7", 6L));
		assertNotNull(folderDao.findByNameAndParentId("test", 6L));
	}

	@Test
	public void testApplyTags() throws ServerException {
		GUIFolder folder = testSubject.getFolder(1201L, true, true, true);
		assertNotNull(folder);
		assertTrue(folder.getTags().isEmpty());

		folder = testSubject.getFolder(1200L, true, true, true);
		assertNotNull(folder);
		folder.addTag("pippo");
		testSubject.save(folder);

		testSubject.applyTags(1200L);
		folder = testSubject.getFolder(1201L, true, true, true);
		assertTrue(folder.getTags().contains("pippo"));
	}

	@Test
	public void testSetFolderPagination() throws ServerException {
		testSubject.setFolderPagination(1200L, 10, 10);
		assertSame(10, session.getDictionary().get(FoldersDataServlet.FOLDER_PAGE_SIZE + ":" + 1200));

		testSubject.setFolderPagination(1200L, null, null);
		assertNull(session.getDictionary().get(FoldersDataServlet.FOLDER_PAGE_SIZE + ":" + 1200));
	}

	@Test
	public void testApplyGridLayout() throws ServerException {
		GUIFolder folder = testSubject.getFolder(1201L, true, true, true);
		assertNotNull(folder);
		assertNull(folder.getGrid());

		folder = testSubject.getFolder(1200L, true, true, true);
		assertNotNull(folder);
		folder.setGrid("grid");
		testSubject.save(folder);

		testSubject.applyGridLayout(1200L);
		folder = testSubject.getFolder(1201L, true, true, true);
		assertEquals("grid", folder.getGrid());
	}

	@Test
	public void testApplyOCR() throws ServerException {
		GUIFolder folder = testSubject.getFolder(1201L, true, true, true);
		assertNotNull(folder);
		assertNull(folder.getGrid());

		folder = testSubject.getFolder(1200L, true, true, true);
		assertNotNull(folder);
		folder.setOcrTemplateId(1L);
		folder.setTemplateId(-1L);
		folder.setTemplate("test");
		testSubject.save(folder);
		folder = testSubject.getFolder(1200L, true, true, true);
		assertEquals(Long.valueOf(1L), folder.getOcrTemplateId());

		folder = testSubject.getFolder(1201L, true, true, true);
		assertNotNull(folder);
		folder.setTemplateId(-1L);
		folder.setTemplate("test");
		testSubject.save(folder);

		testSubject.applyOCR(1200L);

		folder = testSubject.getFolder(1201L, true, true, true);
		assertEquals(Long.valueOf(1L), folder.getOcrTemplateId());
	}

	@Test
	public void testApplyStore() throws ServerException {
		GUIFolder folder = testSubject.getFolder(1201L, true, true, true);
		assertNotNull(folder);
		assertNull(folder.getGrid());

		folder = testSubject.getFolder(1200L, true, true, true);
		assertNotNull(folder);
		folder.setStore(3);
		testSubject.save(folder);

		testSubject.applyStore(1200L);

		folder = testSubject.getFolder(1201L, true, true, true);
		assertSame(3, folder.getStore());
	}

	@Test
	public void testReadImage() throws ServerException, IOException {
		try {
			testSubject.readImage();
			fail("No exception if we do not have any uploaded image?");
		} catch (ServerException e) {
			// All ok
		}

		File imageFile = new File("target/zone.jpg");
		FileUtil.copyResource("zone.jpg", imageFile);
		UploadServlet.cleanUploads(servletSession);
		Map<String, File> uploads = UploadServlet.getUploads(session.getSid());
		uploads.put("zone.jpg", imageFile);

		assertNotNull(testSubject.readImage());
	}

	@Test
	public void testSaveACL() throws ServerException {
		GUIFolder folder = testSubject.getFolder(6, true, true, true);
		int aclBefore = folder.getAccessControlList().size();

		folder.getAccessControlList()
				.add(new GUIAccessControlEntry(4L, Permission.READ.name(), Permission.WRITE.name()));
		testSubject.saveACL(folder, false);

		folder = testSubject.getFolder(6, true, true, true);
		assertFalse(folder.getAccessControlList().isEmpty());
		assertEquals(aclBefore + 1, folder.getAccessControlList().size());
	}

	@Test
	public void testGetAllowedPermissions() throws ServerException, PersistenceException {
		assertEquals(Permission.all().size(),
				testSubject.getAllowedPermissions(List.of(5L, 1200L)).getAllowedPermissions().size());

		prepareSession("boss", "admin");
		assertEquals(0, testSubject.getAllowedPermissions(List.of(5L, 1200L)).getAllowedPermissions().size());
	}

	@Test
	public void testRename() throws ServerException, PersistenceException {
		Folder folder = folderDao.findById(6);
		assertEquals("folder6", folder.getName());

		testSubject.rename(6, "pluto");

		folder = folderDao.findById(6);
		assertEquals("pluto", folder.getName());
	}

	@Test
	public void testApplyRights() throws ServerException, PersistenceException {
		Folder parentFolder = folderDao.findById(6);
		assertNotNull(parentFolder);
		assertTrue(folderDao.isPermissionAllowed(Permission.DELETE, 1201, 3));
		assertTrue(folderDao.isPermissionAllowed(Permission.RENAME, 1201, 3));
		Folder childFolder1 = folderDao.findById(1202);
		assertNotNull(childFolder1);
		assertEquals(1201, childFolder1.getParentId());
		assertTrue(folderDao.isPermissionAllowed(Permission.DELETE, 1202, 3));
		assertTrue(folderDao.isPermissionAllowed(Permission.RENAME, 1202, 3));

		GUIFolder folder = testSubject.getFolder(6, false, false, false);

		testSubject.saveACL(folder, true);

		assertTrue(folderDao.isPermissionAllowed(Permission.DELETE, 1202, 1));
		assertTrue(folderDao.isPermissionAllowed(Permission.RENAME, 1202, 1));
		assertTrue(folderDao.isPermissionAllowed(Permission.DELETE, 1201, 1));
		assertTrue(folderDao.isPermissionAllowed(Permission.RENAME, 1201, 1));
	}

	@Test
	public void testApplyMetadata() throws PersistenceException, ServerException {
		GUIFolder folder9 = testSubject.getFolder(9L, false, false, false);
		assertNull(folder9.getTemplateId());

		Folder parentFolder = folderDao.findById(7L);
		folderDao.initialize(parentFolder);

		TemplateDAO templateDao = Context.get(TemplateDAO.class);
		parentFolder.setTemplate(templateDao.findById(-1L));
		parentFolder.setValue("source", "test");
		folderDao.store(parentFolder);

		testSubject.applyMetadata(7L);

		folder9 = testSubject.getFolder(9L, false, false, false);
		assertEquals(Long.valueOf(-1L), folder9.getTemplateId());
		assertEquals("test", folder9.getValue("source"));
	}

	@Test
	public void testInheritACL() throws ServerException {
		GUIFolder folder4 = testSubject.getFolder(4, false, false, false);
		GUIFolder folder6 = testSubject.getFolder(6L, false, false, false);
		assertEquals(3, folder6.getAccessControlList().size());
		assertNotSame(folder4.getAccessControlList().size(), folder6.getAccessControlList().size());

		testSubject.inheritACL(6L, 4L);

		folder6 = testSubject.getFolder(6, false, false, false);

		testSubject.inheritACL(6L, 4L);
		assertEquals(folder4.getAccessControlList().size(), folder6.getAccessControlList().size());
	}

	@Test
	public void testDelete() throws ServerException {
		GUIFolder folder6 = testSubject.getFolder(6L, false, false, false);
		assertNotNull(folder6);
		GUIFolder folder7 = testSubject.getFolder(7L, false, false, false);
		assertNotNull(folder7);
		GUIFolder folder9 = testSubject.getFolder(9L, false, false, false);
		assertNotNull(folder9);

		testSubject.delete(List.of(6L, 7L));

		folder6 = testSubject.getFolder(6L, false, false, false);
		assertNull(folder6);
		folder7 = testSubject.getFolder(7L, false, false, false);
		assertNull(folder7);
		folder9 = testSubject.getFolder(9L, false, false, false);
		assertNull(folder9);
	}

	@Test
	public void testComputeStats() throws ServerException {
		List<Long> stats = testSubject.computeStats(7L);
		assertEquals(Long.valueOf(1L), stats.get(1));
	}

	@Test
	public void testGetFolder() throws ServerException {
		GUIFolder folder = testSubject.getFolder(6, true, true, true);
		assertNotNull(folder);
		assertEquals("folder6", folder.getName());
		assertEquals(5, folder.getParentId());

		folder = testSubject.getFolder(1202, true, true, true);
		assertNotNull(folder);
		assertEquals("xyz", folder.getName());
		assertEquals(1201, folder.getParentId());
		assertEquals("/test/ABC/xyz", folder.getPathExtended());

		// Try with unexisting id
		folder = null;
		try {
			folder = testSubject.getFolder(9999, true, true, true);
		} catch (ServerException e) {
			// We expect an exception here
		}
		assertNull(folder);

		// Try with an alias
		folder = testSubject.createAlias(5L, 7L);

		folder = testSubject.getFolder(session, folder.getId(), true);
		assertSame(7L, folder.getFoldRef());
	}

	@Test
	public void testCopyFolders() throws ServerException, PersistenceException {
		List<Folder> children = folderDao.findChildren(5L, null);
		int childrenBefore = children.size();

		GUIFolder model = new GUIFolder();
		model.setName("Copied");

		assertFalse(children.stream().anyMatch(c -> c.getName().equals(model.getName())));

		testSubject.copyFolders(List.of(9L), 5L, false, "replicate", model);

		children = folderDao.findChildren(5L, null);

		assertEquals(childrenBefore + 1, children.size());
		assertTrue(children.stream().anyMatch(c -> c.getName().equals(model.getName())));
	}

	@Test
	public void testCreate() throws ServerException, PersistenceException {
		List<Folder> children = folderDao.findChildren(5L, null);
		int childrenBefore = children.size();

		GUIFolder folder = new GUIFolder();
		folder.setName("NewFolder");
		folder.setParentId(5L);

		assertFalse(children.stream().anyMatch(c -> c.getName().equals("NewFolder")));

		folder = testSubject.create(folder, true);
		assertNotNull(folder);

		children = folderDao.findChildren(5L, null);
		assertEquals(childrenBefore + 1, children.size());
		assertTrue(children.stream().anyMatch(c -> c.getName().equals("NewFolder")));

		folder = new GUIFolder();
		folder.setName("NewWorkspace");
		folder.setParentId(5L);

		folder = testSubject.create(folder, true);
		assertNotNull(folder);

		children = folderDao.findChildren(5L, null);
		assertEquals(childrenBefore + 2, children.size());
		assertTrue(children.stream().anyMatch(c -> c.getName().equals("NewWorkspace")));

		folder = new GUIFolder();
		folder.setName("NewFolder3");
		folder.setParentId(5L);
		folder.setFoldRef(9L);
		folder = testSubject.create(folder, true);

		GUIFolder newFolder = new GUIFolder();
		newFolder.setName("NewFolder4");
		newFolder.setParentId(folder.getId());

		newFolder = testSubject.create(newFolder, true);
		assertNotNull(newFolder);
	}

	@Test
	public void testMoveFolderSimple() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderA = folderDao.create(docsFolder, new Folder("folderA"), true, null);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);

		testSubject.move(List.of(folderC.getId()), folderA.getId());

		List<Folder> folderList = folderDao.findChildren(folderA.getId(), null);
		assertEquals(1, folderList.size());

		folderC = folderDao.findById(folderC.getId());
		assertTrue(folderList.contains(folderC));
	}

	@Test
	public void testMoveFolderUp() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderA = folderDao.create(docsFolder, new Folder("folderA"), true, null);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);
		folderDao.create(folderC, new Folder("folderD"), true, null);
		folderDao.create(folderC, new Folder("folderE"), true, null);

		testSubject.move(List.of(folderC.getId()), folderA.getId());

		List<Folder> folderList = folderDao.findChildren(folderA.getId(), null);
		assertEquals(1, folderList.size());
		folderC = folderDao.findById(folderC.getId());
		assertTrue(folderList.contains(folderC));

		folderB = folderDao.findById(folderB.getId());
		folderList = folderDao.findChildren(folderB.getId(), null);
		assertEquals(0, folderList.size());
	}

	@Test
	public void testMoveFolderDown() throws Exception {
		Folder docsFolder = folderDao.findById(Folder.ROOTID);
		Folder folderB = folderDao.create(docsFolder, new Folder("folderB"), true, null);
		Folder folderC = folderDao.create(folderB, new Folder("folderC"), true, null);
		Folder folderD = folderDao.create(folderC, new Folder("folderD"), true, null);
		Folder folderE = folderDao.create(folderC, new Folder("folderE"), true, null);
		folderDao.create(folderE, new Folder("folderF"), true, null);

		testSubject.move(List.of(folderE.getId()), folderD.getId());

		List<Folder> folderList = folderDao.findChildren(folderD.getId(), null);
		assertEquals(1, folderList.size());
		folderE = folderDao.findById(folderE.getId());
		assertTrue(folderList.contains(folderE));

		folderList = folderDao.findChildren(folderC.getId(), null);
		assertEquals(1, folderList.size());
	}

	@Test
	public void testPaste() throws Exception {
		Document doc = documentDao.findById(1L);
		assertEquals(5L, doc.getFolder().getId());

		assertTrue(documentDao.findByFolder(1200L, null).isEmpty());

		testSubject.paste(List.of(1L, 2L, 3L), 1200L, Clipboard.COPY, true, true, true);

		doc = documentDao.findById(1L);
		assertEquals(5L, doc.getFolder().getId());
		assertEquals(3, documentDao.findByFolder(1200L, null).size());

		assertTrue(documentDao.findByFolder(7L, null).isEmpty());

		testSubject.paste(List.of(1L, 2L, 3L), 7L, Clipboard.CUT, true, true, true);

		doc = documentDao.findById(1L);
		assertEquals(7L, doc.getFolder().getId());
		assertEquals(3, documentDao.findByFolder(7L, null).size());
	}

	@Test
	public void testPasteAsAlias() throws Exception {
		Document doc = documentDao.findById(1L);
		assertEquals(5L, doc.getFolder().getId());

		assertTrue(documentDao.findByFolder(1200L, null).isEmpty());

		testSubject.pasteAsAlias(List.of(1L, 2L, 3L), 1200L, null);

		doc = documentDao.findById(1L);
		assertEquals(5L, doc.getFolder().getId());
		assertEquals(3, documentDao.findByFolder(1200L, null).size());

		assertEquals(3, documentDao.findByFolder(1200L, null).size());
		assertTrue(documentDao.findByFolder(1200L, null).stream().allMatch(d -> d.getDocRef() != null));
	}

	@Test
	public void testDeleteFromTrash() throws Exception {
		assertEquals(1, folderDao.queryForInt("select ld_deleted from ld_folder where ld_id=" + 8));

		testSubject.deleteFromTrash(List.of(8L));

		assertEquals(2, folderDao.queryForInt("select ld_deleted from ld_folder where ld_id=" + 8));
	}

	@Test
	public void testRestore() throws Exception {
		assertEquals(8L, folderDao.queryForLong("select ld_id from ld_folder where ld_id=" + 8L + " and ld_deleted=1"));

		testSubject.restore(List.of(8L), 1200L);
		Folder folder = folderDao.findById(8L);
		folderDao.initialize(folder);
		assertNotNull(folder);
		assertEquals(1200L, folder.getParentId());
	}
}