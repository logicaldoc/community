package com.logicaldoc.core.folder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.FolderAccessControlEntry;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link HibernateFolderDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class HibernateFolderDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private FolderDAO testSubject;

	private UserDAO userDao;

	private DocumentDAO docDao;

	private DocumentManager docManager;

	private FolderHistoryDAO historyDao;

	private TemplateDAO templateDao;

	private static final Logger log = LoggerFactory.getLogger(HibernateFolderDAOTest.class);

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateFolderDAO
		testSubject = Context.get(FolderDAO.class);
		userDao = Context.get(UserDAO.class);
		docDao = Context.get(DocumentDAO.class);
		historyDao = Context.get(FolderHistoryDAO.class);
		templateDao = Context.get(TemplateDAO.class);
		docManager = Context.get(DocumentManager.class);
	}

	@Test
	public void testCreatePath() throws Exception {
		Folder docsFolder = testSubject.findById(6);
		Folder folder = testSubject.createPath(docsFolder, "/pippo/pluto/paperino", true, null);
		assertEquals("paperino", folder.getName());
		folder = testSubject.findById(folder.getParentId());
		assertEquals("pluto", folder.getName());
		folder = testSubject.findById(folder.getParentId());
		assertEquals("pippo", folder.getName());

		folder = testSubject.createPath(docsFolder, "/pippo/pluto/paperino", true, null);
		assertEquals("paperino", folder.getName());
		folder = testSubject.findById(folder.getParentId());
		assertEquals("pluto", folder.getName());
		folder = testSubject.findById(folder.getParentId());
		assertEquals("pippo", folder.getName());

		folder = testSubject.createPath(testSubject.findDefaultWorkspace(Tenant.DEFAULT_ID),
				"/pippo/research & development", true, null);
		assertNotNull(folder);
		assertEquals("research & development", folder.getName());
		Folder pippo = testSubject.findByName("pippo", 1L).get(0);
		assertNotNull(pippo);
		Folder research = testSubject.findByName("research & development", 1L).get(0);
		assertNotNull(research);

		Folder f = testSubject.findByPathExtended("/Default/pippo/research & development", 1L);
		assertNotNull(f);
		assertEquals(folder.getId(), f.getId());

		folder = testSubject.createPath(testSubject.findDefaultWorkspace(Tenant.DEFAULT_ID),
				"/pippo/research  development", true, null);
		assertNotNull(folder);
		assertEquals("research  development", folder.getName());

		folder = testSubject.createPath(docsFolder, "/Capo d'Orlando 05-35632/Comunicazioni ingresso", true, null);
		assertNotNull(folder);
		folder = testSubject.findById(folder.getParentId());
		assertEquals("Capo d'Orlando 05-35632", folder.getName());

		/*
		 * Test a folder with a template
		 */
		folder = new Folder();
		folder.setName("withTemplate");
		folder.setTemplate(templateDao.findByName("email", Tenant.DEFAULT_ID));
		folder.setValue("from", "test@acme.com");
		folder.setParentId(testSubject.findDefaultWorkspace(Tenant.DEFAULT_ID).getId());
		testSubject.store(folder);

		folder = testSubject.createPath(folder, "/A/B/C/D/E/F/G/H/I/L/M/N/O/P/Q/R/S/T/U/V/Z", false, null);
		assertNotNull(folder);
		f = testSubject.findByPathExtended("/Default/withTemplate/A/B/C/D/E/F/G/H/I/L/M/N/O/P", 1L);
		testSubject.initialize(f);
		assertEquals("P", f.getName());
		assertEquals("test@acme.com", f.getValue("from"));
	}

	@Test
	public void testCount() throws PersistenceException {
		int docCount = testSubject.count(false);
		int docCountDelete = testSubject.count(true);
		assertEquals(6, docCount);
		assertEquals(9, docCountDelete);
	}

	@Test
	public void testFind() throws PersistenceException {
		Folder folder = testSubject.findByPathExtended("/test", 1L);
		assertNotNull(folder);
		assertEquals("test", folder.getName());
		assertEquals(1200, folder.getId());

		folder = testSubject.findByPathExtended("/test/ABC/xyz", 1L);
		assertNotNull(folder);
		assertEquals("xyz", folder.getName());
		assertEquals(1202, folder.getId());

		folder = testSubject.findByPathExtended("/test/ABC/qqq", 1L);
		assertNull(folder);

		folder = testSubject.findById(1210L);
		testSubject.initialize(folder);
		assertEquals(2, folder.getStores().keySet().size());
		assertEquals(3, folder.getStore().intValue());

		List<Folder> folders = testSubject.find("folder", 1L);
		assertNotNull(folders);
		assertEquals(6, folders.size());
		folder = testSubject.findById(6);
		Folder folder2 = testSubject.findById(7);
		assertTrue(folders.contains(folder));
		assertTrue(folders.contains(folder2));

		folders = testSubject.find("folder.adminxx", 1L);
		assertEquals(0, folders.size());
		folders = testSubject.find("folder", 99L);
		assertEquals(0, folders.size());

		// Folder with foldRef
		Folder folderWithRef = testSubject.findFolder(1301);
		assertNotNull(folderWithRef);

		Folder referencedFolder = testSubject.findFolder(1301);
		assertNotNull(referencedFolder);
		assertEquals(777, referencedFolder.getId());

		// Non-existent folder
		Folder nonExistentFolder = testSubject.findFolder(99999);
		assertNull(nonExistentFolder);
	}

	@Test
	public void testCountDocsInTree() throws PersistenceException {
		/*
		 * Make sure to compute all the paths
		 */
		List<Folder> folders = testSubject.findAll();
		for (Folder folder : folders) {
			if (folder.getPath() == null) {
				testSubject.initialize(folder);
				folder.setPath(testSubject.computePath(folder));
				testSubject.store(folder);
			}
		}

		long count = testSubject.countDocsInTree(5L);
		assertEquals(6, count);

		count = testSubject.countDocsInTree(4L);
		assertEquals(0, count);
	}

	@Test
	public void testCountDocs() throws PersistenceException {
		long count = testSubject.countDocs(6L);
		assertEquals(6, count);
	}

	@Test
	public void testComputeTreeSize() throws PersistenceException {
		/*
		 * Make sure to compute all the paths
		 */
		List<Folder> folders = testSubject.findAll();
		for (Folder folder : folders) {
			if (folder.getPath() == null) {
				testSubject.initialize(folder);
				folder.setPath(testSubject.computePath(folder));
				testSubject.store(folder);
			}
		}

		long size = testSubject.computeTreeSize(5L);
		assertEquals(635739L, size);

		size = testSubject.computeTreeSize(4L);
		assertEquals(0, size);
	}

	@Test
	public void testDeleteTree() throws Exception {
		assertNotNull(testSubject.findById(1200));
		assertNotNull(testSubject.findById(1202));
		User user = new User();
		user.setUsername("admin");
		user.setId(1);
		FolderHistory transaction = new FolderHistory();
		transaction.setUser(user);
		testSubject.deleteTree(1200L, PersistentObject.DELETED_CODE_DEFAULT, transaction);
		assertNull(testSubject.findById(1200));

		boolean runOk = false;
		try {
			testSubject.deleteTree(null, 1, new FolderHistory(transaction));
			runOk = true;
		} catch (PersistenceException e) {
			assertEquals("No folder was specified", e.getMessage());
		}

		assertFalse(runOk);

		runOk = false;
		try {
			testSubject.deleteTree(testSubject.findById(1200), 0, new FolderHistory(transaction));
			runOk = true;
		} catch (PersistenceException e) {
			assertEquals("Deletion code cannot be 0", e.getMessage());
		}
		assertFalse(runOk);

		// Delete an alias
		Folder alias = testSubject.createAlias(4L, 6, new FolderHistory(transaction));
		assertNotNull(alias);
		testSubject.deleteTree(alias, 1, new FolderHistory(transaction));
		assertNull(testSubject.findById(alias.getId()));
	}

	@Test
	public void testCopy() throws Exception {
		// Create a tree and populate it
		Folder defaultWorkspace = testSubject.findById(Folder.DEFAULTWORKSPACEID);
		Folder newFolder = testSubject.createPath(defaultWorkspace, "pippo/pluto", true, null);
		assertNotNull(newFolder);

		Folder source = testSubject.findByPathExtended("/Default/pippo", Tenant.DEFAULT_ID);
		assertNotNull(source);

		User user = userDao.findByUsername("admin");

		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		doc.setCustomId(null);
		doc.setFolder(source);
		assertNotNull(docManager.create(new FileInputStream("pom.xml"), doc, transaction));

		doc = docDao.findById(1);
		docDao.initialize(doc);
		doc.setCustomId(null);
		doc.setFolder(testSubject.findByPathExtended("/Default/pippo/pluto", Tenant.DEFAULT_ID));
		assertNotNull(docManager.create(new FileInputStream("pom.xml"), doc, transaction));

		FolderHistory tr = new FolderHistory();
		tr.setNotified(0);
		tr.setComment("");
		tr.setUser(user);

		/*
		 * Now create a target folder and copy there inside
		 */
		Folder target = testSubject.createPath(defaultWorkspace, "target", true, null);
		testSubject.initialize(target);
		assertNotNull(target);
		target.setTemplate(templateDao.findByName("email", Tenant.DEFAULT_ID));
		target.setValue("from", "test@acme.com");
		testSubject.store(target);
		target = testSubject.findById(target.getId());
		testSubject.initialize(target);
		assertEquals("email", target.getTemplate().getName());
		assertEquals("test@acme.com", target.getValue("from"));

		testSubject.copy(source, target, null, false, "inherit", tr);

		Folder folder = testSubject.findByPathExtended("/Default/target/pippo/pluto", Tenant.DEFAULT_ID);
		testSubject.initialize(target);
		assertNotNull(folder);
		testSubject.initialize(folder);
		assertEquals("email", folder.getTemplate().getName());
		assertEquals("test@acme.com", folder.getValue("from"));

		List<Document> docs = docDao.findByFolder(folder.getId(), null);
		assertEquals(1, docs.size());

		folder = testSubject.findById(6L);
		testSubject.initialize(folder);
		assertEquals(3, folder.getAccessControlList().size());

		newFolder = testSubject.copy(folder, testSubject.findById(1210L), null, false, "replicate", tr);
		testSubject.initialize(newFolder);
		assertEquals(3, newFolder.getAccessControlList().size());

		// Copy folder structure and preserving security references
		defaultWorkspace = testSubject.findById(Folder.DEFAULTWORKSPACEID);
		testSubject.createPath(defaultWorkspace, "A/B/C/D", false, null);

		Folder B = testSubject.findByPathExtended("/Default/A/B", 1L);
		assertNull(B.getSecurityRef());

		Folder C = testSubject.findByPathExtended("/Default/A/B/C", 1L);
		testSubject.initialize(C);
		assertNull(C.getSecurityRef());
		C.setSecurityRef(B.getId());
		testSubject.store(C);

		Folder D = testSubject.findByPathExtended("/Default/A/B/C/D", 1L);
		testSubject.initialize(D);
		assertNull(D.getSecurityRef());
		D.setSecurityRef(B.getId());
		testSubject.store(D);

		target = testSubject.createPath(defaultWorkspace, "TARGET", false, null);
		source = testSubject.findByPathExtended("/Default/A", 1L);

		tr = new FolderHistory();
		tr.setNotified(0);
		tr.setComment("");
		tr.setUser(userDao.findByUsername("admin"));
		testSubject.copy(source, target, null, false, "replicate", tr);

		B = testSubject.findByPathExtended("/Default/TARGET/A/B", 1L);
		C = testSubject.findByPathExtended("/Default/TARGET/A/B/C", 1L);
		D = testSubject.findByPathExtended("/Default/TARGET/A/B/C/D", 1L);
		assertEquals(B.getId(), C.getSecurityRef().longValue());
		assertEquals(B.getId(), D.getSecurityRef().longValue());
	}

	@Test
	public void testMoveFolderSimple() throws Exception {
		Folder docsFolder = testSubject.findById(Folder.DEFAULTWORKSPACEID);
		Folder folderVO = new Folder();
		folderVO.setName("folderA");
		Folder folderA = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderB");
		Folder folderB = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderC");
		Folder folderC = testSubject.create(folderB, folderVO, true, null);

		User user = userDao.findByUsername("admin");

		FolderHistory transaction = new FolderHistory();
		transaction.setNotified(0);
		transaction.setComment("");
		transaction.setUser(user);

		testSubject.move(folderC, folderA, transaction);

		List<Folder> folderList = testSubject.findChildren(folderA.getId(), null);
		assertEquals(1, folderList.size());

		assertTrue(folderList.contains(folderC));
	}

	@Test
	public void testMoveFolderUp() throws Exception {
		Folder docsFolder = testSubject.findById(6);
		Folder folderVO = new Folder();
		folderVO.setName("folderA");
		Folder folderA = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderB");
		Folder folderB = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderC");
		Folder folderC = testSubject.create(folderB, folderVO, true, null);
		folderVO.setName("folderD");
		testSubject.create(folderC, folderVO, true, null);
		folderVO.setName("folderE");
		testSubject.create(folderC, folderVO, true, null);

		User user = userDao.findByUsername("admin");

		FolderHistory transaction = new FolderHistory();
		transaction.setNotified(0);
		transaction.setComment("");
		transaction.setUser(user);

		testSubject.move(folderC, folderA, transaction);

		List<Folder> folderList = testSubject.findChildren(folderA.getId(), null);
		assertEquals(1, folderList.size());
		assertTrue(folderList.contains(folderC));

		folderList = testSubject.findChildren(folderB.getId(), null);
		assertEquals(0, folderList.size());

		// Move folder up with documents
		docsFolder = testSubject.findById(6);
		folderVO = new Folder();
		folderVO.setName("folderA");
		folderA = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderB");
		folderB = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderC");
		folderC = testSubject.create(folderB, folderVO, true, null);
		folderVO.setName("folderD");
		Folder folderD = testSubject.create(folderC, folderVO, true, null);
		folderVO.setName("folderE");
		testSubject.create(folderC, folderVO, true, null);

		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		doc.setFolder(folderC);
		doc.setIndexingStatus(IndexingStatus.INDEXED);
		docDao.store(doc);

		Document doc2 = docDao.findById(2);
		docDao.initialize(doc2);
		doc2.setFolder(folderD);
		doc2.setIndexingStatus(IndexingStatus.INDEXED);
		docDao.store(doc2);

		user = userDao.findByUsername("admin");

		transaction = new FolderHistory();
		transaction.setNotified(0);
		transaction.setComment("");
		transaction.setUser(user);

		testSubject.move(folderC, folderA, transaction);
		Folder fc = testSubject.findById(folderC.getId());
		assertEquals(folderC.getName(), fc.getName());

		folderList = testSubject.findChildren(folderA.getId(), null);
		assertEquals(1, folderList.size());

		assertTrue(folderList.contains(folderC));

		folderList = testSubject.findChildren(folderB.getId(), null);
		assertEquals(0, folderList.size());

		List<Document> docs = docDao.findByIndexingStatus(IndexingStatus.TO_INDEX);
		assertEquals(1, docs.size());

		// Check the history creation
		List<FolderHistory> folderFolderHistory = historyDao.findByFolderId(folderC.getId());
		assertTrue(folderFolderHistory.size() > 0);

		boolean eventPresent = false;
		for (FolderHistory history : folderFolderHistory) {
			if (FolderEvent.MOVED.equals(history.getEventEnum()))
				eventPresent = true;
		}
		assertTrue(eventPresent);
	}

	@Test
	public void testMoveFolderDown() throws Exception {
		Folder docsFolder = testSubject.findById(6L);

		Folder folderVO = new Folder();
		folderVO.setName("folderB");
		Folder folderB = testSubject.create(docsFolder, folderVO, true, null);
		folderVO.setName("folderC");
		Folder folderC = testSubject.create(folderB, folderVO, true, null);
		folderVO.setName("folderD");
		Folder folderD = testSubject.create(folderC, folderVO, true, null);
		folderVO.setName("folderE");
		Folder folderE = testSubject.create(folderC, folderVO, true, null);
		folderVO.setName("folderF");
		testSubject.create(folderE, folderVO, true, null);

		User user = userDao.findByUsername("admin");

		FolderHistory transaction = new FolderHistory();
		transaction.setNotified(0);
		transaction.setComment("");
		transaction.setUser(user);

		testSubject.move(folderE, folderD, transaction);

		List<Folder> folderList = testSubject.findChildren(folderD.getId(), null);
		assertEquals(1, folderList.size());
		assertTrue(folderList.contains(folderE));

		folderList = testSubject.findChildren(folderC.getId(), null);
		assertEquals(1, folderList.size());
	}

	@Test
	public void testSetCreatorInCaseCreationEvent() throws PersistenceException {
		Folder docsFolder = testSubject.findById(6L);
		Folder folderVO = new Folder();
		folderVO.setName("folderBVO");

		FolderHistory transaction = new FolderHistory();
		transaction.setEvent((FolderEvent) null);

		Folder folderB = testSubject.create(docsFolder, folderVO, true, transaction);
		folderB.setName("folderB");
		testSubject.store(folderB);

		assertEquals("folderB", folderB.getName());

		folderVO = new Folder();
		folderVO.setName("folderBVO");
		folderVO.setParentId(6L);

		transaction = new FolderHistory();
		testSubject.store(folderVO, transaction);

		assertEquals(FolderEvent.CREATED, transaction.getEventEnum());
	}

	@Test
	public void testIsInPath() throws Exception {
		assertTrue(testSubject.isInPath(1200, 1201));
		assertTrue(testSubject.isInPath(1200, 1202));
		assertFalse(testSubject.isInPath(99, 1202));
	}

	@Test
	public void testStore() throws PersistenceException {
		Folder folder = new Folder();
		folder.setName("text");
		folder.setParentId(5);
		folder.setAccessControlList(Set.of(new FolderAccessControlEntry(1L), new FolderAccessControlEntry(2L)));

		folder.addTag("A");

		// Add a very big tag
		folder.addTag(StringUtils.repeat("B", 500));

		testSubject.store(folder);

		FolderHistory transaction = new FolderHistory();
		transaction.setFolderId(folder.getId());
		transaction.setUser(userDao.findById(1));
		transaction.setNotified(0);
		Folder alias = testSubject.createAlias(5L, folder.getId(), transaction);
		assertNotNull(alias);

		folder.clearTags();

		Set<String> words = Set.of("C", "DDDDDDDDDDDD");
		folder.setTagsFromWords(words);
		assertEquals("[C, DDDDDDDDDDDD]", folder.getTagsAsWords().toString());

		// Test updating the security rules
		folder = testSubject.findById(folder.getId());
		testSubject.initialize(folder);

		assertEquals(2, folder.getAccessControlList().size());

		FolderAccessControlEntry ace = new FolderAccessControlEntry();
		ace.setGroupId(3L);
		ace.grantPermissions(
				Set.of(Permission.READ, Permission.WRITE, Permission.DELETE, Permission.MOVE, Permission.DOWNLOAD));
		folder.addAccessControlEntry(ace);

		ace = new FolderAccessControlEntry();
		ace.setGroupId(4L);
		ace.grantPermissions(Permission.forGuests());
		folder.addAccessControlEntry(ace);

		ace = new FolderAccessControlEntry();
		ace.setGroupId(10L);
		ace.grantPermissions(Permission.match("read", "download", "add"));
		folder.addAccessControlEntry(ace);

		testSubject.store(folder);

		folder = testSubject.findById(folder.getId());
		testSubject.initialize(folder);
		assertEquals(5, folder.getAccessControlList().size());

		AccessControlEntry face = folder.getAccessControlEntry(4L);

		assertTrue(face.grantedPermissions().contains(Permission.PREVIEW));
		assertFalse(face.grantedPermissions().contains(Permission.WRITE));

		face = folder.getAccessControlEntry(10L);
		assertTrue(face.grantedPermissions().contains(Permission.ADD));
		assertTrue(face.grantedPermissions().contains(Permission.DOWNLOAD));
		assertFalse(face.grantedPermissions().contains(Permission.PREVIEW));

		// Set a securityRef
		folder.setSecurityRef(5L);
		testSubject.store(folder);

		// Check if aliases have been updates
		alias = testSubject.findById(alias.getId());
		assertEquals(folder.getSecurityRef(), alias.getSecurityRef());

		// Delete the folder
		testSubject.delete(folder.getId());

		// Check aliases were deleted too
		assertTrue(testSubject.findAliases(folder.getId(), folder.getTenantId()).isEmpty());

		folder = new Folder();
		folder.setName("unexisting parent");
		folder.setParentId(99999L);

		try {
			transaction.setSessionId("xxxxx");
			testSubject.store(folder, transaction);
			fail("No error here, but should be");
		} catch (PersistenceException e) {
			assertEquals("Unexisting parent folder " + folder.getParentId(), e.getMessage());
		}

		folder = testSubject.findById(6);
		testSubject.initialize(folder);
		assertEquals("folder6", folder.getName());

		// Load an existing folder and modify it
		folder = testSubject.findById(6);
		testSubject.initialize(folder);
		assertEquals("folder6", folder.getName());

		transaction = new FolderHistory();
		transaction.setFolderId(folder.getId());
		transaction.setEvent(FolderEvent.RENAMED);
		transaction.setUser(userDao.findById(1));
		transaction.setNotified(0);
		testSubject.store(folder, transaction);

		assertEquals(FolderEvent.RENAMED, transaction.getEventEnum());
		assertFalse(folder.isAlias());

		folder = testSubject.findById(7);
		testSubject.initialize(folder);
		testSubject.store(folder);

		folder = testSubject.findById(7);
		testSubject.initialize(folder);
		folder.setName("xxxx");
		transaction = new FolderHistory();
		transaction.setFolderId(folder.getId());
		transaction.setEvent(FolderEvent.RENAMED);
		transaction.setUser(userDao.findById(1));
		transaction.setNotified(0);
		testSubject.store(folder, transaction);

		folder = testSubject.findById(6);
		testSubject.initialize(folder);
		folder.getAccessControlList().remove(folder.getAccessControlEntry(2));
		assertEquals(2, folder.getAccessControlList().size());
		testSubject.store(folder);

		folder = testSubject.findById(Folder.ROOTID);
		testSubject.initialize(folder);
		assertEquals(4, folder.getAccessControlList().size());

		folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		folder.setName("pippo");
		testSubject.store(folder);
		assertNotNull(folder);
		folder = testSubject.findById(1202);
		assertNotNull(folder);

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		folder.setName("pippo2");
		testSubject.store(folder);
		assertNotNull(folder);

		// Try a folder with extended attributes
		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		folder.setStore(9);
		assertEquals("test_val_1", folder.getValue("val1"));
		folder.setValue("val1", "xyz");
		testSubject.store(folder);
		assertNotNull(folder);
		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		assertEquals("xyz", folder.getValue("val1"));

		Folder folder999 = new Folder("TestFolder");
		assertEquals("TestFolder", folder999.getName());
	}

	@Test
	public void testStoreValidAndDefaultWorkspaceScenarios() {
		// Store a valid workspace
		Folder workspaceFolder = new Folder();
		testSubject.initialize(workspaceFolder);
		workspaceFolder.setId(0L);
		workspaceFolder.setType(Folder.TYPE_WORKSPACE);
		workspaceFolder.setParentId(5);
		workspaceFolder.setName("Workspace");

		try {
			testSubject.store(workspaceFolder);
		} catch (Exception e) {
			fail("store() should not have thrown an exception, but it threw: " + e.getMessage());
		}

		// Store workspace when root is null
		Folder folder = new Folder();
		folder.setId(560L);
		folder.setType(Folder.TYPE_WORKSPACE);
		folder.setTenantId(9999999L);
		folder.setParentId(5);
		folder.setName("Workspace");

		try {
			testSubject.store(folder);
			assertEquals("Workspace", folder.getName());
		} catch (Exception e) {
			fail("store() should not throw exception when root is null, but threw: " + e.getMessage());
		}

		// Store a default workspace with the same name
		Folder defaultWorkspace = new Folder();
		defaultWorkspace.setId(4L);
		defaultWorkspace.setType(Folder.TYPE_WORKSPACE);
		defaultWorkspace.setTenantId(1L);
		defaultWorkspace.setParentId(5);
		defaultWorkspace.setName(Folder.DEFAULTWORKSPACENAME);

		try {
			testSubject.store(defaultWorkspace);
			assertNotNull(defaultWorkspace);
			assertEquals(Folder.DEFAULTWORKSPACENAME, defaultWorkspace.getName());
		} catch (Exception e) {
			fail("store() should not have thrown an exception, but it threw: " + e.getMessage());
		}
	}

	@Test
	public void testStoreWorkspaceShouldThrowException() throws PersistenceException {
		// Moving a workspace should throw PersistenceException
		Folder movedWorkspace = new Folder();
		movedWorkspace.setId(560L);
		movedWorkspace.setType(Folder.TYPE_WORKSPACE);
		movedWorkspace.setTenantId(1L);
		movedWorkspace.setParentId(999L);
		movedWorkspace.setName("Workspace");

		try {
			testSubject.store(movedWorkspace);
			fail("Expected PersistenceException to be thrown");
		} catch (PersistenceException e) {
			// Exception expected
		}

		// Renaming default workspace should throw PersistenceException
		Folder renamedWorkspace = new Folder();
		renamedWorkspace.setId(4L);
		renamedWorkspace.setType(Folder.TYPE_WORKSPACE);
		renamedWorkspace.setTenantId(1L);
		renamedWorkspace.setParentId(5);
		renamedWorkspace.setName("RenamedWorkspace");

		try {
			testSubject.store(renamedWorkspace);
			fail("Expected PersistenceException to be thrown");
		} catch (PersistenceException e) {
			// Exception expected
		}
	}

	@Test
	public void testCreate() throws PersistenceException {
		Folder parent = testSubject.findById(1202L);

		Folder folderVO = new Folder();
		folderVO.setName("xxxx");
		Folder folder = testSubject.create(parent, folderVO, true, null);
		assertNotNull(folder);

		folder = testSubject.findById(folder.getId());
		testSubject.initialize(folder);
		assertEquals("test1", folder.getTemplate().getName());
		assertEquals("test_val_1", folder.getValue("val1"));
		assertEquals("xxxx(" + folder.getId() + ")", folder.toString());
	}

	@Test
	public void testCreateAlias() throws PersistenceException {
		Folder alias = testSubject.createAlias(4L, 3000L, null);
		assertNotNull(alias);

		assertEquals(Long.valueOf(3000L), alias.getFoldRef());
		assertEquals(Long.valueOf(3000L), alias.getSecurityRef());
		assertEquals(Integer.valueOf(Folder.TYPE_ALIAS), Integer.valueOf(alias.getType()));
		Folder orig = testSubject.findById(3000L);
		assertEquals("Workspace X", orig.getName());
		assertEquals(Folder.TYPE_WORKSPACE, orig.getType());
	}

	@Test
	public void testFindAliases() throws PersistenceException {
		List<Folder> aliases = testSubject.findAliases(3000L, 1L);
		assertTrue(aliases.isEmpty());

		Folder alias = testSubject.createAlias(4L, 3000L, null);
		assertNotNull(alias);

		aliases = testSubject.findAliases(3000L, 1L);
		assertEquals(1, aliases.size());
		assertEquals(alias, aliases.iterator().next());
	}

	@Test
	public void testDelete() throws PersistenceException {
		Folder folder = testSubject.findById(1202L);
		assertNotNull(folder);

		testSubject.delete(1202L);
		folder = testSubject.findById(1202L);
		assertNull(folder);

		docDao.delete(1202);

		// Delete a folder with documents
		testSubject.delete(1201);
		folder = testSubject.findById(1201);
		assertNull(folder);

		try {
			testSubject.delete(Folder.DEFAULTWORKSPACEID);
			fail("No exception was thrown");
		} catch (PersistenceException e) {
			log.debug("Delete error on DEFAULTWORKSPACEID", e);
		}

		folder = testSubject.findById(Folder.DEFAULTWORKSPACEID);
		assertNotNull(folder);
	}

	@Test
	public void testDeleteAll() throws PersistenceException {
		List<Folder> folders = testSubject.findByParentId(1200);
		assertEquals(3, folders.size());

		User user = userDao.findByUsername("admin");

		FolderHistory transaction = new FolderHistory();
		transaction.setNotified(0);
		transaction.setComment("");
		transaction.setUser(user);

		testSubject.deleteAll(folders, 1, transaction);

		folders = testSubject.findByParentId(1200);
		assertTrue(folders.isEmpty());
	}

	@Test
	public void testDeleteWithAliases() throws PersistenceException {
		List<Folder> aliasesBefore = testSubject.findAliases(1500L, 1L);
		assertNotNull(aliasesBefore);
		assertFalse(aliasesBefore.isEmpty());

		testSubject.delete(1500);

		Folder folder = testSubject.findById(1500);
		assertNull(folder);

		Folder alias1 = testSubject.findById(1501);
		Folder alias2 = testSubject.findById(1502);

		assertNull(alias1);
		assertNull(alias2);
	}

	@Test
	public void testFindById() throws PersistenceException {
		// Try with a folder id
		Folder folder = testSubject.findById(Folder.ROOTID);
		testSubject.initialize(folder);
		assertNotNull(folder);
		assertEquals(Folder.ROOTID, folder.getId());
		assertEquals("/", folder.getName());
		assertEquals(4, folder.getAccessControlList().size());

		Folder sameFolder = testSubject.findById(Folder.ROOTID);
		testSubject.initialize(sameFolder);
		assertEquals(folder, sameFolder);
		assertEquals(folder.hashCode(), sameFolder.hashCode());

		// Try with non-existing id
		folder = testSubject.findById(99999);
		assertNull(folder);

		// Try a folder with extended attributes
		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		assertNotNull(folder);
		assertEquals("test_val_1", folder.getValue("val1"));

		Folder differentFolder = new Folder("DifferentFolder");
		assertNotSame(folder, differentFolder);
		assertNotSame(folder.hashCode(), differentFolder.hashCode());
	}

	@Test
	public void testFindDeleted() throws PersistenceException {
		// Try with a folder id
		List<Folder> folders = testSubject.findDeleted(3, 100);
		assertEquals(3, folders.size());

		folders = testSubject.findDeleted(99, 100);
		assertEquals(0, folders.size());
	}

	@Test
	public void testFindByName() throws PersistenceException {
		// Try with existing text
		List<Folder> folders = testSubject.findByName("%folder%", null);
		assertNotNull(folders);
		assertEquals(4, folders.size());

		folders = (List<Folder>) testSubject.findByName(null, "test", null, true);
		assertNotNull(folders);
		assertEquals(1, folders.size());

		Folder parent = testSubject.findById(Folder.ROOTID);
		folders = (List<Folder>) testSubject.findByName(parent, "test", null, true);
		assertNotNull(folders);
		assertEquals(1, folders.size());

		// Try with non-existing text
		folders = testSubject.findByName("xxxxx", null);
		assertNotNull(folders);
		assertTrue(folders.isEmpty());

		// Try with umlauts
		assertNotNull(testSubject.findByName(testSubject.findById(Folder.DEFAULTWORKSPACEID), "Elard", null, false));
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		Folder alias = testSubject.createAlias(1200L, 6L, null);
		assertNotNull(alias);

		List<Folder> folders = testSubject.findByUserId(1L);
		assertNotNull(folders);
		assertEquals(16, folders.size());

		alias = testSubject.createAlias(1200L, 6L, null);
		assertNotNull(alias);

		testSubject.createAlias(6L, 1201L, null);
		folders = testSubject.findByUserId(4L);
		assertNotNull(folders);
		assertEquals(11, folders.size());

		folders = testSubject.findByUserId(1, Folder.ROOTID);
		assertNotNull(folders);
		assertEquals(3, folders.size());

		// Existing user and non-existent folder
		folders = testSubject.findByUserId(1, 70);
		assertNotNull(folders);
		assertEquals(0, folders.size());

		// Existing user without group
		User user = new User();
		user.setUsername("userWithNoGroup");
		user.setGroups(new HashSet<>());
		userDao.store(user);

		folders = testSubject.findByUserId(user.getId(), 1200L);
		assertNotNull(folders);

		// Non-existing user
		String runOk = null;
		try {
			testSubject.findByUserId(99, Folder.ROOTID);
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertNull(runOk);

		// Existing user with no folders associated
		folders = testSubject.findByUserId(2);
		assertEquals(0, folders.size());

		folders = testSubject.findByUserId(4, Folder.ROOTID);
		assertNotNull(folders);
		assertEquals(1, folders.size());

		// user author has id 4 and group id 2
		User user4 = userDao.findById(4L);
		assertFalse(user4.isAdmin());

		Folder dependantFoler = new Folder();
		// Make security depending on default workspace
		dependantFoler.setSecurityRef(Folder.DEFAULTWORKSPACEID);
		dependantFoler.setName("Dummy");
		dependantFoler.setParentId(Folder.DEFAULTWORKSPACEID);
		testSubject.store(dependantFoler);

		Folder workspace = testSubject.findById(Folder.DEFAULTWORKSPACEID);
		testSubject.initialize(workspace);
		workspace.getAccessControlList().add(new FolderAccessControlEntry(user4.getGroups().iterator().next().getId()));
		testSubject.store(workspace);

		folders = testSubject.findByUserId(user4.getId(), Folder.DEFAULTWORKSPACEID);
		assertNotNull(folders);

		folders = testSubject.findByUserId(user4.getId());
		assertNotNull(folders);

		// Find by user id with security reference
		folders = testSubject.findByUserId(4);
		assertFalse(folders.isEmpty());

		boolean containsFolder4 = folders.stream().anyMatch(f -> f.getId() == 4);
		assertTrue(containsFolder4);

		folders = testSubject.findByUserId(4);

		boolean contains1302 = folders.stream().anyMatch(f -> f.getId() == 1302);
		assertTrue(contains1302);
	}

	@Test
	public void testFindByParentId() {
		List<Folder> folders = testSubject.findByParentId(Folder.ROOTID);
		assertNotNull(folders);
		assertEquals(14, folders.size());

		// Try with non-existing parent
		folders = testSubject.findByParentId(999);
		assertNotNull(folders);
		assertEquals(0, folders.size());
	}

	@Test
	public void testIsWriteEnabled() throws PersistenceException {
		assertTrue(testSubject.isWriteAllowed(Folder.ROOTID, 1));
		assertTrue(testSubject.isWriteAllowed(6, 1));
		assertTrue(testSubject.isWriteAllowed(1200, 3));
		assertTrue(testSubject.isWriteAllowed(Folder.ROOTID, 3));

		// non-existing user
		String runOk = null;
		try {
			assertFalse(testSubject.isWriteAllowed(Folder.ROOTID, 99));
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertNull(runOk);
	}

	@Test
	public void testIsDownloadEnabled() throws PersistenceException {
		assertTrue(testSubject.isDownloadllowed(Folder.ROOTID, 1));
		assertTrue(testSubject.isDownloadllowed(6, 1));
		assertTrue(testSubject.isDownloadllowed(1200, 3));
		assertTrue(testSubject.isDownloadllowed(Folder.ROOTID, 3));
		assertFalse(testSubject.isDownloadllowed(1200, 4));
	}

	@Test
	public void testIsMoveEnabled() throws PersistenceException {
		assertTrue(testSubject.isMoveAllowed(Folder.ROOTID, 1));
		assertTrue(testSubject.isMoveAllowed(6, 1));
		assertTrue(testSubject.isMoveAllowed(1200, 3));
		assertTrue(testSubject.isMoveAllowed(Folder.ROOTID, 3));
		assertFalse(testSubject.isMoveAllowed(1200, 4));
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		assertNotNull(testSubject.findByPathExtended("/Default/Elard", 1L));
		assertNull(testSubject.findByPathExtended("/Default/Elard", 99L));
	}

	@Test
	public void testIsReadEnabled() throws PersistenceException {
		assertTrue(testSubject.isReadAllowed(Folder.ROOTID, 1));
		assertTrue(testSubject.isReadAllowed(5, 1));

		boolean runOk = false;
		try {
			assertFalse(testSubject.isReadAllowed(Folder.ROOTID, 22));
			runOk = true;
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 22", e.getMessage());
		}
		assertFalse(runOk);

		runOk = false;
		try {
			// Non-existing user
			assertFalse(testSubject.isReadAllowed(Folder.ROOTID, 99));
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertFalse(runOk);

		assertTrue(testSubject.isReadAllowed(1200L, 3L));
		assertFalse(testSubject.isReadAllowed(1200L, 4L));
		assertTrue(testSubject.isReadAllowed(1201L, 4L));

		Folder folder = testSubject.findById(6L);
		testSubject.initialize(folder);
		folder.setSecurityRef(1201L);
		testSubject.store(folder);
		assertTrue(testSubject.isReadAllowed(6L, 4L));

		// Non-existing folder
		assertFalse(testSubject.isReadAllowed(999999L, 4L));

		// User with no group
		User user = new User();
		user.setUsername("userWithNoGroup");

		userDao.store(user);

		User storedUser = userDao.findByUsername("userWithNoGroup");
		assertNotNull(storedUser);
		assertEquals(false, testSubject.isReadAllowed(6L, user.getId()));
	}

	@Test
	public void testIsPrintEnable() throws PersistenceException {
		assertTrue(testSubject.isPrintAllowed(Folder.ROOTID, 1L));
		assertTrue(testSubject.isPrintAllowed(5, 1));

		String runOk = null;
		try {
			testSubject.isPrintAllowed(Folder.ROOTID, 22L);
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 22", e.getMessage());
		}
		assertNull(runOk);

		try {
			testSubject.isPrintAllowed(Folder.ROOTID, 999L);
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 999", e.getMessage());
		}
		assertNull(runOk);

		assertTrue(testSubject.isPrintAllowed(1200, 3));
	}

	@Test
	public void testIsPermissionEnabled() throws PersistenceException {
		assertTrue(testSubject.isPermissionAllowed(Permission.WRITE, Folder.ROOTID, 1));
		assertTrue(testSubject.isPermissionAllowed(Permission.WRITE, 6, 1));
		assertFalse(testSubject.isPermissionAllowed(Permission.WRITE, Folder.ROOTID, 5));

		// Non-existing user
		String runOk = null;
		try {
			assertFalse(testSubject.isPermissionAllowed(Permission.WRITE, Folder.ROOTID, 99));
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertNull(runOk);

		assertTrue(testSubject.isPermissionAllowed(Permission.WRITE, 6, 4));
	}

	@Test
	public void testIsPreviewEnabled() throws PersistenceException {
		assertEquals(true, testSubject.isPreviewAllowed(Folder.ROOTID, 1));
	}

	@Test
	public void testGetEnabledPermissions() throws PersistenceException {
		Set<Permission> permissions = testSubject.getAllowedPermissions(Folder.ROOTID, 1);
		assertEquals(Permission.all().size(), permissions.size());
		assertTrue(permissions.contains(Permission.READ));
		assertTrue(permissions.contains(Permission.SECURITY));
		assertTrue(permissions.contains(Permission.SIGN));

		permissions = testSubject.getAllowedPermissions(6, 4);
		assertEquals(11, permissions.size());
		assertTrue(permissions.contains(Permission.READ));
		assertTrue(permissions.contains(Permission.WRITE));
		assertTrue(permissions.contains(Permission.MOVE));
		assertTrue(permissions.contains(Permission.EMAIL));

		permissions = testSubject.getAllowedPermissions(999, 1);
		assertEquals(Permission.all().size(), permissions.size());

		permissions = testSubject.getAllowedPermissions(1301, 4);
		assertNotNull(permissions);

		// Create a user with no groups
		User user = new User();
		user.setUsername("userWithoutGroups");
		user.setGroups(new HashSet<>());
		userDao.store(user);

		permissions = testSubject.getAllowedPermissions(1202, user.getId());
		assertNotNull(permissions);
		assertTrue(permissions.isEmpty());
	}

	@Test
	public void testFindFolderIdByUserId() throws PersistenceException {
		Collection<Long> ids = testSubject.findFolderIdByUserId(3, null, true);
		assertNotNull(ids);
		assertEquals(6, ids.size());

		// Try with non-existing user
		String runOk = null;
		try {
			testSubject.findFolderIdByUserId(99, null, true);
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertNull(runOk);
	}

	@Test
	public void testFindIdByUserId() throws PersistenceException {
		Collection<Long> ids = testSubject.findIdByUserId(1, 1201);
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertTrue(ids.contains(1202L));

		ids = testSubject.findIdByUserId(3, 1200);
		assertNotNull(ids);
		assertEquals(2, ids.size());

		ids = testSubject.findIdByUserId(4, 1200);
		assertNotNull(ids);
		assertEquals(2, ids.size());

		ids = testSubject.findIdByUserId(1, 1201);
		assertNotNull(ids);
		assertEquals(1, ids.size());

		// Try with non-existing user
		String runOk = null;
		try {
			testSubject.findIdByUserId(99, 1201);
			runOk = "ok";
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertNull(runOk);

		// security-referenced folders
		ids = testSubject.findIdByUserId(3, 3000);
		assertNotNull(ids);
		assertTrue(ids.contains(1301L));

		ids = testSubject.findIdByUserId(3, 3000);
		assertEquals(4, ids.size());
	}

	@Test
	public void testFindFolderIdByUserIdInPath() throws PersistenceException {
		Collection<Long> ids = testSubject.findFolderIdByUserIdInPath(1, Long.valueOf(1200));
		assertEquals(6, ids.size());
		assertTrue(ids.contains(1201L));

		ids = testSubject.findFolderIdByUserIdInPath(4, Long.valueOf(1200));
		assertEquals(4, ids.size());
		assertTrue(ids.contains(1201L));

		ids = testSubject.findFolderIdByUserIdInPath(4, Long.valueOf(1202));
		assertEquals(1, ids.size());

		// Try with non-existing user
		boolean runOk = false;
		try {
			testSubject.findFolderIdByUserIdInPath(99, Long.valueOf(1200));
			runOk = true;
		} catch (PersistenceException e) {
			assertEquals("Unexisting user 99", e.getMessage());
		}
		assertFalse(runOk);

		// Try with non-existing folder
		runOk = false;
		try {
			testSubject.findFolderIdByUserIdInPath(4, Long.valueOf(999));
			runOk = true;
		} catch (PersistenceException e) {
			assertEquals("Unexisting folder 999", e.getMessage());
		}
		assertFalse(runOk);
	}

	@Test
	public void testFindTags() throws PersistenceException {
		List<String> tags = testSubject.findTags(1200);
		assertEquals(2, tags.size());
		assertTrue(tags.contains("ftag1"));

		tags = testSubject.findTags(6);
		assertTrue(tags.isEmpty());
	}

	@Test
	public void testFindIdByParentId() {
		Collection<Long> ids = testSubject.findIdsByParentId(1200);
		assertNotNull(ids);
		assertEquals(3, ids.size());
		assertTrue(ids.contains(1202L));

		ids = testSubject.findIdsByParentId(1201);
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertTrue(ids.contains(1202L));

		ids = testSubject.findIdsByParentId(1204);
		assertNotNull(ids);
		assertTrue(ids.isEmpty());
	}

	@Test
	public void testHasWriteAccess() throws PersistenceException {
		Folder folder = testSubject.findById(1200);
		assertTrue(testSubject.hasWriteAccess(folder, 3));
		assertFalse(testSubject.hasWriteAccess(folder, 5));
		folder = testSubject.findById(6);
		assertTrue(testSubject.hasWriteAccess(folder, 3));
	}

	@Test
	public void testFindByGroupId() throws PersistenceException {
		Collection<Folder> folders = testSubject.findByGroupId(1);
		assertEquals(15, folders.size());
		folders = testSubject.findByGroupId(10);
		assertEquals(0, folders.size());
		folders = testSubject.findByGroupId(2);
		assertEquals(6, folders.size());
	}

	@Test
	public void testFindParents() throws PersistenceException {
		List<Folder> folders = testSubject.findParents(1202);
		assertEquals(3, folders.size());
		assertEquals(testSubject.findById(Folder.ROOTID), folders.get(0));
		assertEquals(testSubject.findById(1200), folders.get(1));
		assertEquals(testSubject.findById(1201), folders.get(2));

		// Non-existing folder id
		folders = testSubject.findParents(99999);
		assertNotNull(folders);
		assertEquals(true, folders.isEmpty());
	}

	@Test
	public void testFindWorkspace() throws PersistenceException {
		Folder folder = testSubject.findWorkspace(Folder.DEFAULTWORKSPACEID);
		assertTrue(folder.isWorkspace());
		assertEquals("Default", folder.getName());

		folder = testSubject.findWorkspace(6L);
		assertTrue(folder.isWorkspace());
		assertEquals("Workspace X", folder.getName());

		// find default workspace
		folder = testSubject.findDefaultWorkspace(1L);
		assertNotNull(folder);
		assertEquals(Folder.DEFAULTWORKSPACENAME, folder.getName());
		assertEquals(Folder.DEFAULTWORKSPACEID, folder.getId());

		folder = testSubject.findDefaultWorkspace(99L);
		assertNull(folder);
	}

	@Test
	public void testFindWorkspaces() throws PersistenceException {
		List<Folder> dirs = testSubject.findWorkspaces(1L);
		assertNotNull(dirs);
		assertEquals(2, dirs.size());
		assertEquals("Default", dirs.get(0).getName());

		dirs = testSubject.findWorkspaces(99L);
		assertNotNull(dirs);
		assertEquals(0, dirs.size());
	}

	@Test
	public void testRestore() throws PersistenceException {
		assertEquals(1204L,
				testSubject.queryForLong("select ld_id from ld_folder where ld_id=" + 1204L + " and ld_deleted=1"));

		testSubject.restore(1204L, 5L, null);

		assertEquals(1204L,
				testSubject.queryForLong("select ld_id from ld_folder where ld_id=" + 1204L + " and ld_deleted=0"));

		Folder folder = testSubject.findById(1204L);
		assertNotNull(folder);

		FolderHistory transaction = new FolderHistory();
		// added folder with id 1300 and ld_deleted = 1
		testSubject.restore(1300, 1201, transaction);

		Folder restoredFolder = testSubject.findById(1300);
		assertEquals(FolderEvent.RESTORED, transaction.getEventEnum());
		assertNotNull(restoredFolder);
		assertNotSame(1, restoredFolder.getDeleted());

		Folder rootFolder = testSubject.findRoot(restoredFolder.getTenantId());
		assertEquals(rootFolder.getId(), restoredFolder.getParentId());
	}

	@Test
	public void testFindByNameAndParentId() throws PersistenceException {
		List<Folder> folders = testSubject.findByNameAndParentId("%folder%", 3000L);
		assertEquals(3, folders.size());
		assertFalse(folders.contains(testSubject.findById(99)));
		assertTrue(folders.contains(testSubject.findById(7)));
		folders = testSubject.findByNameAndParentId("ABC", 1200);
		assertEquals(testSubject.findById(1201), folders.get(0));
	}

	@Test
	public void testFindFolderIdByUserIdAndPermission() throws PersistenceException {
		Collection<Long> ids = testSubject.findFolderIdByUserIdAndPermission(5, Permission.WRITE, null, true);
		assertNotNull(ids);
		assertEquals(3, ids.size());

		ids = testSubject.findFolderIdByUserIdAndPermission(3, Permission.WRITE, null, true);
		assertNotNull(ids);
		assertEquals(5, ids.size());

		ids = testSubject.findFolderIdByUserIdAndPermission(4, Permission.WRITE, 1200L, true);
		assertNotNull(ids);
		assertEquals(3, ids.size());

		ids = testSubject.findFolderIdByUserIdAndPermission(User.USERID_ADMIN, Permission.WRITE, 1200L, true);
		assertNotNull(ids);
		assertEquals(4, ids.size());

		ids = testSubject.findFolderIdByUserIdAndPermission(User.USERID_ADMIN, Permission.WRITE, 1200L, false);
		assertNotNull(ids);
		assertEquals(3, ids.size());
	}

	@Test
	public void testFindByUserIdAndTag() throws PersistenceException {
		Folder folder = new Folder();
		folder.setName("text");
		folder.setParentId(5);
		folder.addTag("xyz");
		testSubject.store(folder);

		List<Folder> foldewrs = testSubject.findByUserIdAndTag(1L, "xyz", null);
		assertNotNull(foldewrs);
		assertEquals(1, foldewrs.size());

		foldewrs = testSubject.findByUserIdAndTag(4L, "ftag3", null);
		assertEquals(1, foldewrs.size());
		assertTrue(foldewrs.contains(testSubject.findById(1201)));

		foldewrs = testSubject.findByUserIdAndTag(1L, "unexisting", null);
		assertNotNull(foldewrs);
		assertEquals(0, foldewrs.size());
	}

	@Test
	public void testComputePathExtended() throws PersistenceException {
		assertEquals("/", testSubject.computePathExtended(5L));
		assertEquals("/test/ABC", testSubject.computePathExtended(1201));
		assertNull(testSubject.computePathExtended(null));

		// when root == null
		Folder testFolder = new Folder();
		testFolder.setTenantId(99);
		assertNull(testSubject.computePathExtended(testFolder));
	}

	@Test
	public void testComputePath() throws PersistenceException {
		assertNull(testSubject.computePath(null));
		assertEquals("/", testSubject.computePath(5L));
		assertEquals("/1200/1201", testSubject.computePath(1201));

		Folder folderNonRoot = new Folder();
		folderNonRoot.setId(109);
		folderNonRoot.setParentId(109);
		folderNonRoot.setPath("109");
		testSubject.computePath(folderNonRoot);
		assertEquals("/109", testSubject.computePath(folderNonRoot));
	}

	@Test
	public void testFindChildren() throws PersistenceException {
		List<Folder> dirs = testSubject.findChildren(1200L, 1L);
		assertNotNull(dirs);
		assertEquals(2, dirs.size());

		Folder alias = testSubject.createAlias(1201L, 6L, null);
		assertNotNull(alias);

		dirs = testSubject.findChildren(1201L, 3L);
		assertNotNull(dirs);
		assertEquals(2, dirs.size());

		dirs = testSubject.findChildren(1201L, 4L);
		assertNotNull(dirs);
		assertEquals(2, dirs.size());
	}

	@Test
	public void testApplySecurityToTree() throws PersistenceException {
		FolderHistory transaction = new FolderHistory();
		User user = new User();
		user.setId(4);
		transaction.setUser(user);
		transaction.setNotified(0);

		// The root defines it's own policies
		Folder folder = testSubject.findById(1200);
		assertNull(folder.getSecurityRef());

		testSubject.applySecurityToTree(1200L, transaction);

		folder = testSubject.findById(1201);
		assertEquals(1200L, folder.getSecurityRef().longValue());
		folder = testSubject.findById(1202);
		assertEquals(1200L, folder.getSecurityRef().longValue());

		// The root refers to another folder's policies
		folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		folder.setSecurityRef(5L);
		testSubject.store(folder);
		testSubject.applySecurityToTree(1200L, transaction);

		folder = testSubject.findById(1201);
		assertEquals(5L, folder.getSecurityRef().longValue());
		folder = testSubject.findById(1202);
		assertEquals(5L, folder.getSecurityRef().longValue());
	}

	@Test
	public void testApplyTagsToTree() throws PersistenceException {
		FolderHistory transaction = new FolderHistory();
		User user = new User();
		user.setId(4);
		transaction.setUser(user);
		transaction.setNotified(0);

		// The root defines it's own policies
		Folder folder = testSubject.findById(1200L);
		testSubject.initialize(folder);

		assertTrue(folder.getTagsAsWords().contains("ftag1"));

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		assertEquals(1, folder.getTags().size());
		assertTrue(folder.getTagsAsWords().contains("ftag3"));

		testSubject.applyTagsToTree(1200L, transaction);
		testSubject.findById(1201);
		testSubject.initialize(folder);
		assertTrue(folder.getTagsAsWords().contains("ftag1"));
	}

	@Test
	public void testUpdateSecurityRef() throws PersistenceException {
		FolderHistory transaction = new FolderHistory();
		User user = new User();
		user.setId(4);
		transaction.setUser(user);
		transaction.setNotified(0);

		// The root defines it's own policies
		Folder folder = testSubject.findById(1200);
		assertNull(folder.getSecurityRef());
		testSubject.applySecurityToTree(1200, transaction);

		folder = testSubject.findById(1201L);
		assertEquals(1200L, folder.getSecurityRef().longValue());
		assertEquals(1200L, folder.getParentId());
		folder = testSubject.findById(1202L);
		assertEquals(1200L, folder.getSecurityRef().longValue());
		assertEquals(1201L, folder.getParentId());

		// The root has its own policies
		testSubject.updateSecurityRef(1200L, 5L, transaction);

		folder = testSubject.findById(1200L);
		assertEquals(5L, folder.getSecurityRef().longValue());
		folder = testSubject.findById(1201L);
		assertEquals(5L, folder.getSecurityRef().longValue());
		folder = testSubject.findById(1202L);
		assertEquals(5L, folder.getSecurityRef().longValue());
	}

	@Test
	public void testApplyMetadataToTree() throws PersistenceException {
		FolderHistory transaction = new FolderHistory();
		User user = new User();
		user.setId(4);
		transaction.setUser(user);
		transaction.setNotified(0);

		Folder folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		assertNull(folder.getTemplate());
		folder = testSubject.findById(1201);
		assertEquals(1L, folder.getTemplate().getId());
		folder = testSubject.findById(1202);
		assertNotNull(folder.getTemplate());

		folder = testSubject.findById(1200);
		testSubject.initialize(folder);

		Template template = templateDao.findById(1L);
		folder.setTemplate(template);
		folder.setValue("attr1", "test");
		testSubject.store(folder);

		testSubject.applyMetadataToTree(1200L, transaction);
		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		assertTrue(testSubject.isInPath(1200L, 1202L));
		assertEquals("test1", folder.getTemplate().getName());
		assertEquals("test", folder.getValue("attr1"));

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		assertEquals("test1", folder.getTemplate().getName());
		assertEquals("test", folder.getValue("attr1"));

		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		assertEquals("test1", folder.getTemplate().getName());
		assertEquals("test", folder.getValue("attr1"));

		folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		folder.getAttributes().clear();
		folder.setTemplate(null);
		testSubject.store(folder);
		testSubject.applyMetadataToTree(1200L, transaction);

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		assertEquals(null, folder.getTemplate());
		folder = testSubject.findById(1202);
		testSubject.initialize(folder);
		assertEquals(null, folder.getTemplate());
	}

	@Test
	public void testFindFolderIdInTree() {
		Collection<Long> ids = testSubject.findFolderIdInTree(1200L, false);
		assertEquals(4, ids.size());
		assertTrue(ids.contains(1202L));
		assertTrue(ids.contains(1201L));
		assertTrue(ids.contains(1200L));

		ids.clear();
		ids = testSubject.findFolderIdInTree(5L, false);
		assertEquals(15, ids.size());
		assertTrue(ids.contains(1200L));
		assertTrue(ids.contains(6L));
		assertTrue(ids.contains(3000L));

		ids.clear();
		ids = testSubject.findFolderIdInTree(1200L, false);
		assertEquals(4, ids.size());
		assertTrue(ids.contains(1201L));
	}

	@Test
	public void testFindbyPathExtended() throws PersistenceException {
		Folder folder = testSubject.findByPathExtended("/Default/Elard", 1L);
		assertNotNull(folder);
	}

	@Test
	public void testMerge() throws PersistenceException, FileNotFoundException {
		Folder root = testSubject.findById(5L);
		testSubject.createPath(root, "/Default/Target/Pippo", false, null);
		testSubject.createPath(root, "/Default/Target/Pluto", false, null);
		testSubject.createPath(root, "/Default/Target/Pluto/Paperino", false, null);
		testSubject.createPath(root, "/Default/Target/Pluto/Paperina", false, null);

		testSubject.createPath(root, "/Default/Source/Pippo", false, null);
		testSubject.createPath(root, "/Default/Source/Pluto", false, null);
		testSubject.createPath(root, "/Default/Source/Pluto/Paperino", false, null);
		testSubject.createPath(root, "/Default/Source/Pluto/Paperina", false, null);
		testSubject.createPath(root, "/Default/Source/Pollo/ABC", false, null);
		testSubject.createPath(root, "/Default/Source/Pollo/DEF", false, null);

		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		transaction.setUserId(1L);
		transaction.setNotified(0);

		FolderHistory fTransaction = new FolderHistory();
		fTransaction.setUser(user);
		fTransaction.setUserId(1L);
		fTransaction.setNotified(0);

		Document doc = docDao.findById(1);
		assertNotNull(doc);

		docDao.initialize(doc);
		doc = new Document(doc);
		doc.setId(0);
		doc.setFolder(testSubject.findByPathExtended("/Default/Target/Pippo", 1L));
		doc.setFileName("doc1.txt");
		docManager.create(new FileInputStream("pom.xml"), doc, transaction);

		doc = new Document(doc);
		doc.setId(0);
		doc.setFolder(testSubject.findByPathExtended("/Default/Source/Pippo", 1L));
		doc.setFileName("doc1.txt");
		docManager.create(new FileInputStream("pom.xml"), doc, transaction);

		doc = new Document(doc);
		doc.setId(0);
		doc.setFolder(testSubject.findByPathExtended("/Default/Source/Pippo", 1L));
		doc.setFileName("doc2.txt");
		docManager.create(new FileInputStream("pom.xml"), doc, transaction);

		doc = new Document(doc);
		doc.setId(0);
		doc.setFolder(testSubject.findByPathExtended("/Default/Source/Pluto/Paperina", 1L));
		doc.setFileName("doc3.txt");
		docManager.create(new FileInputStream("pom.xml"), doc, transaction);

		doc = new Document(doc);
		doc.setId(0);
		doc.setFolder(testSubject.findByPathExtended("/Default/Source/Pollo/DEF", 1L));
		doc.setFileName("doc4.txt");
		docManager.create(new FileInputStream("pom.xml"), doc, transaction);

		Folder target = testSubject.findByPathExtended("/Default/Target", 1L);
		Folder source = testSubject.findByPathExtended("/Default/Source", 1L);
		testSubject.merge(source, target, fTransaction);

		source = testSubject.findByPathExtended("/Default/Source", 1L);
		assertTrue(source == null || source.getDeleted() != 0);

		target = testSubject.findByPathExtended("/Default/Target", 1L);
		assertNotNull(target);

		Folder folder = testSubject.findByPathExtended("/Default/Target/Pollo/ABC", 1L);
		assertTrue(folder == null || folder.getDeleted() != 0);
		folder = testSubject.findByPathExtended("/Default/Target/Pollo/DEF", 1L);
		assertTrue(folder == null || folder.getDeleted() != 0);

		docDao.findByPath("/Default/Target/Pollo/DEF/doc4.txt", 1L);

		try {
			testSubject.merge(null, target, fTransaction);
		} catch (PersistenceException e) {
			// catch exception
		}

		source = testSubject.findById(3000L);
		assertNotNull(source);
		try {
			testSubject.merge(source, null, fTransaction);
		} catch (PersistenceException e) {
			// catch exception
		}
	}

	@Test
	public void testApplyGridToTree() throws PersistenceException {
		Folder folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		assertNull(folder.getGrid());
		folder.setGrid("xyz");
		testSubject.store(folder);
		folder = testSubject.findById(1200);
		assertEquals("xyz", folder.getGrid());

		folder = testSubject.findById(1201);
		assertNull(folder.getGrid());

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(userDao.findById(User.USERID_ADMIN));
		transaction.setNotified(0);
		testSubject.applyGridToTree(1200, transaction);

		folder = testSubject.findById(1201);
		assertEquals("xyz", folder.getGrid());
	}

	@Test
	public void testApplyStoreToTree() throws PersistenceException {
		Folder folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		assertNull(folder.getStore());
		folder.setStore(1);
		testSubject.store(folder);
		folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		assertEquals(Integer.valueOf(1), folder.getStore());

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		assertNull(folder.getStore());

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(userDao.findById(User.USERID_ADMIN));
		transaction.setNotified(0);
		testSubject.applyStoreToTree(1200, transaction);

		folder = testSubject.findById(1201);
		testSubject.initialize(folder);
		assertEquals(Integer.valueOf(1), folder.getStore());
	}

	@Test
	public void testApplyOCRToTree() throws PersistenceException {
		Folder folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		assertNull(folder.getOcrTemplateId());
		folder.setOcrTemplateId(1L);
		testSubject.store(folder);
		folder = testSubject.findById(1200);

		// Without a template the OCR template also must be forced to null
		assertNull(folder.getOcrTemplateId());

		TemplateDAO tDao = Context.get(TemplateDAO.class);
		folder = testSubject.findById(1200);
		testSubject.initialize(folder);
		folder.setTemplate(tDao.findById(1));
		assertNull(folder.getOcrTemplateId());
		folder.setOcrTemplateId(1L);
		testSubject.store(folder);
		assertEquals(Long.valueOf(1), folder.getOcrTemplateId());

		folder = testSubject.findById(1201);
		assertNull(folder.getOcrTemplateId());

		FolderHistory transaction = new FolderHistory();
		transaction.setUser(userDao.findById(User.USERID_ADMIN));
		transaction.setNotified(0);
		testSubject.applyOCRToTree(1200, transaction);

		folder = testSubject.findById(1201);
		assertEquals(Long.valueOf(1), folder.getOcrTemplateId());
	}

	@Test
	public void testCompareTo() {
		Folder folder1 = new Folder("FolderABC");
		testSubject.initialize(folder1);
		folder1.setPosition(1);
		folder1.setPathExtended("/logicaldoc-core/target");
		assertEquals(0, folder1.compareTo(folder1));

		Folder folder2 = new Folder("FolderBCD");
		folder2.setPosition(2);
		assertEquals(-1, Integer.signum(folder1.compareTo(folder2)));
		assertEquals(1, Integer.signum(folder2.compareTo(folder1)));

		Folder folder3 = new Folder("FolderC");
		folder3.setPosition(1);
		Folder folder4 = new Folder("FolderB");
		folder4.setPosition(1);
		assertEquals(1, Integer.signum(folder3.compareTo(folder4)));
		assertEquals(-1, Integer.signum(folder4.compareTo(folder3)));

		assertEquals("/logicaldoc-core/target", folder1.getPathExtended());
	}

	@Test
	public void testEqualsNameNullAndOtherNotNull() {
		// Name is null and other not null
		Folder folder991 = new Folder();
		folder991.setName(null);
		folder991.setParentId(1);

		Folder folder992 = new Folder();
		folder992.setName("NotNullName");
		folder992.setParentId(1);

		assertEquals(false, folder991.equals(folder992));

		// both names are null
		Folder folder1 = new Folder();
		folder1.setName(null);
		folder1.setParentId(1);

		Folder folder2 = new Folder();
		folder2.setName(null);
		folder2.setParentId(1);
		assertEquals(true, folder1.equals(folder2));
	}

	@Test
	public void testGetQuotaAlertRecipientsAsList() {
		Folder folder = new Folder();

		// Null value
		folder.setQuotaAlertRecipients(null);
		assertEquals("[]", folder.getQuotaAlertRecipientsAsList().toString());

		// Empty string
		folder.setQuotaAlertRecipients("");
		assertEquals("", folder.getQuotaAlertRecipients());

		folder.setQuotaAlertRecipients("user1@example.com, user2@example.com, user3@example.com");
		List<String> result = folder.getQuotaAlertRecipientsAsList();
		assertEquals(3, result.size());
		assertEquals("user1@example.com", result.get(0));
		assertEquals("user2@example.com", result.get(1));
		assertEquals("user3@example.com", result.get(2));
	}

	@Test
	public void testAddQuotaAlertRecipient() {
		Folder folder = new Folder();

		folder.setQuotaAlertRecipients("user@example.com");
		folder.addQuotaAlertRecipient("");
		folder.addQuotaAlertRecipient(null);
		List<String> result = folder.getQuotaAlertRecipientsAsList();
		assertEquals(1, result.size());
		assertEquals("user@example.com", result.get(0));

		folder.setQuotaAlertRecipients("user1@example.com");
		folder.addQuotaAlertRecipient("user2@example.com");
		folder.addQuotaAlertRecipient("user3@example.com");
		result = folder.getQuotaAlertRecipientsAsList();
		assertEquals(3, result.size());
	}

	@Test
	public void testBeforeStore() throws PersistenceException {
		FolderValidator folderValidator = new FolderValidator();
		Map<String, Object> dictionary = new HashMap<>();
		Folder folder = new Folder();
		FolderHistory transaction = new FolderHistory();

		dictionary.put("validated", "true");

		folderValidator.beforeStore(folder, transaction, dictionary);
		assertEquals("true", dictionary.get("validated"));

		FolderValidator validator = new FolderValidator();

		// transaction is null
		dictionary = new HashMap<>();
		folder = new Folder();
		transaction = null;

		validator.beforeStore(folder, transaction, dictionary);
		assertEquals("true", dictionary.get("validated"));

		// transaction event is not "CHANGED" or "CREATED"
		dictionary = new HashMap<>();
		folder = new Folder();
		transaction = new FolderHistory();
		transaction.setEvent(FolderEvent.CHANGED);
		validator.beforeStore(folder, transaction, dictionary);
		assertEquals("true", dictionary.get("validated"));

		// validate
		folderValidator = new FolderValidator();
		dictionary = new HashMap<>();
		folder = new Folder();

		Template template = new Template();
		folder.setTemplate(template);

		transaction = new FolderHistory();
		transaction.setEvent(FolderEvent.CHANGED);

		dictionary.put("validated", "false");
		folderValidator.beforeStore(folder, transaction, dictionary);
		assertEquals("true", dictionary.get("validated"));

		folderValidator.afterStore(folder, transaction, dictionary);
	}
}