package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.MockStore;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link DocumentManager}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public class DocumentManagerTest extends AbstractCoreTestCase {

	private DocumentDAO docDao;

	private VersionDAO verDao;

	private UserDAO userDao;

	private FolderDAO folderDao;

	private DocumentNoteDAO documentNoteDao;

	private DocumentLinkDAO documentLinkDao;

	private MockStore store;

	private Store store2;

	// Instance under test
	private DocumentManager testSubject;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		docDao = DocumentDAO.get();
		verDao = VersionDAO.get();
		userDao = UserDAO.get();
		folderDao = FolderDAO.get();
		documentNoteDao = DocumentNoteDAO.get();
		documentLinkDao = DocumentLinkDAO.get();
		store = Context.get(MockStore.class);
		store2 = Context.get(Store.class);

		testSubject = Context.get(DocumentManager.class);
	}

	@Test
	public void testUpdate() throws PersistenceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals("pippo.pdf", doc.getFileName());

		Document newDoc = docDao.findById(2);
		assertNotNull(newDoc);
		assertEquals("pluto", newDoc.getFileName());

		docDao.initialize(doc);
		docDao.initialize(newDoc);

		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		newDoc.setCustomId("xxxxxxxx");

		testSubject.update(doc, newDoc, transaction);
		assertEquals("pluto(1)", doc.getFileName());
		assertEquals("1.1", doc.getVersion());

		assertEquals("1.1", verDao.queryForString("select ld_version from ld_version where ld_documentid=" + doc.getId()
				+ " and ld_version='" + doc.getVersion() + "'"));

		// Document is null
		Document doc1 = docDao.findById(99999);
		assertNull(doc1);
		try {
			testSubject.update(doc1, null, transaction);
		} catch (IllegalArgumentException e) {
			// catching exception
		}

		// DocumentVO is null
		try {
			testSubject.update(doc, null, transaction);
		} catch (IllegalArgumentException e) {
			// catching exception
		}

		/*
		 * Template is null and docVO templateID != null and docVO has extended
		 * attributes
		 */
		doc.setTemplate(null);
		newDoc.setTemplateId(1L);

		Map<String, Attribute> attributes = new HashMap<>();
		Attribute ext1 = new Attribute();
		attributes.put("attr1", ext1);
		Attribute ext2 = new Attribute();
		attributes.put("attr1", ext2);
		newDoc.setAttributes(attributes);

		testSubject.update(doc, newDoc, transaction);
		assertNotSame(0, newDoc.getTemplateId());
	}

	@Test
	public void testCreateTicket() throws PersistenceException, PermissionException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);

		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		transaction.setUserId(1L);
		transaction.setNotified(0);

		Ticket t = new Ticket();
		t.setDocId(1L);
		t = testSubject.createTicket(t, transaction);
		assertNotNull(t.getUrl());

		t = new Ticket();
		t.setDocId(1L);
		t.setExpireHours(2);
		t = testSubject.createTicket(t, transaction);
		assertNotNull(t.getUrl());

		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.DATE, -2);
		t = new Ticket();
		t.setDocId(1L);
		t.setExpired(cal.getTime());
		t = testSubject.createTicket(t, transaction);
		assertNotNull(t.getUrl());
		assertTrue(t.isTicketExpired());

		t = new Ticket();
		t.setDocId(1L);
		t.setType(Ticket.VIEW);
		t = testSubject.createTicket(t, transaction);
		assertNotNull(t.getUrl());

		// Non-existing document
		boolean exceptionHappened = false;
		try {
			t = new Ticket();
			t.setDocId(99L);
			testSubject.createTicket(t, transaction);
		} catch (PersistenceException e) {
			exceptionHappened = true;
			assertEquals("Unexisting document 99", e.getMessage());
		}
		assertTrue(exceptionHappened);

		// No download permission
		exceptionHappened = false;
		try {
			transaction = new DocumentHistory();
			User userWithourPermission = userDao.findByUsername("sebastian");
			transaction.setUser(userWithourPermission);

			userDao.jdbcUpdate("delete from ld_folder_acl where ld_folderid=" + doc.getFolder().getId());
			userDao.jdbcUpdate("delete from ld_usergroup where ld_groupid=" + Group.GROUPID_ADMIN);

			assertFalse(folderDao.isDownloadllowed(doc.getFolder().getId(), userWithourPermission.getId()));

			t = new Ticket();
			t.setDocId(1L);
			t.setExpireHours(2);
			testSubject.createTicket(t, transaction);
		} catch (PermissionException e) {
			exceptionHappened = true;
			assertTrue(e.getMessage().contains("does not have permission"));
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testCopyToFolder() throws PersistenceException, IOException {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		Folder folder = doc.getFolder();
		assertEquals(6, folder.getId());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		transaction.setFilename(doc.getFileName());

		Folder newFolder = folderDao.findById(6);
		docDao.initialize(doc);

		List<DocumentLink> originalLinks = documentLinkDao.findByDocId(doc.getId());
		assertFalse(originalLinks.isEmpty());

		List<DocumentNote> originalNotes = documentNoteDao.findByDocId(doc.getId(), User.USERID_ADMIN, null);
		assertFalse(originalNotes.isEmpty());

		try {
			store.setUseDummyFile(true);
			Document newDoc = testSubject.copyToFolder(doc, newFolder, transaction, false, false, true).getDocument();
			assertNotSame(doc.getId(), newDoc.getId());
			assertEquals(newFolder, newDoc.getFolder());
			assertTrue(documentLinkDao.findByDocId(newDoc.getId()).isEmpty());
			assertTrue(documentNoteDao.findByDocId(newDoc.getId(), User.USERID_ADMIN, null).isEmpty());
		} finally {
			store.setUseDummyFile(false);
		}

		try {
			store.setUseDummyFile(true);
			Document newDoc = testSubject.copyToFolder(doc, newFolder, transaction, true, true, false).getDocument();
			assertNotSame(doc.getId(), newDoc.getId());
			assertEquals(newFolder, newDoc.getFolder());

			List<DocumentLink> links = documentLinkDao.findByDocId(newDoc.getId());
			assertEquals(originalLinks.size(), links.size());
			for (DocumentLink link : links) {
				assertNotNull(link.getDocument1());
				assertNotNull(link.getDocument2());
				assertTrue(
						newDoc.getId() == link.getDocument1().getId() || newDoc.getId() == link.getDocument2().getId());
			}

			List<DocumentNote> notes = documentNoteDao.findByDocId(newDoc.getId(), User.USERID_ADMIN, null);
			assertEquals(originalNotes.size(), notes.size());
			for (DocumentNote note : notes) {
				assertEquals(newDoc.getId(), note.getDocId());
				assertEquals(newDoc.getFileVersion(), note.getFileVersion());
			}
		} finally {
			store.setUseDummyFile(false);
		}

		doc = docDao.findById(2);
		assertNotNull(doc);
		testSubject.copyToFolder(doc, newFolder, new DocumentHistory(transaction), true, true, false);
	}

	@Test
	public void testCountPages() throws PersistenceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals(5, doc.getPages());
		assertEquals(55, testSubject.countPages(doc));
	}

	@Test
	public void testMoveToFolder() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		Folder folder = doc.getFolder();
		assertEquals(6, folder.getId());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(6L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		Folder newFolder = folderDao.createPath(folderDao.findById(Folder.ROOTID), "/Default/test", true, null);
		testSubject.moveToFolder(doc, newFolder, transaction);

		doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals(1L, doc.getId());
		assertEquals(newFolder, doc.getFolder());

		// folder is equal to doc.getfolder
		doc.setFolder(folder);
		testSubject.moveToFolder(doc, folder, transaction);
	}

	@Test
	public void testParseDocument() throws PersistenceException, ParsingException {
		Document doc = docDao.findById(1);
		String text = testSubject.parseDocument(doc, null);
		assertTrue(text.contains("dolor"));

		Folder folder = folderDao.createPath(folderDao.findById(Folder.ROOTID), "/Default/test", true, null);

		// Try with an alias
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		Document alias = testSubject.createAlias(doc, folder, null, transaction);
		text = testSubject.parseDocument(alias, null);
		assertTrue(text.contains("dolor"));

		Document doc1 = new Document();
		doc1.setFileName("testDoc");
		doc1.setFolder(folder);
		docDao.store(doc1);

		doc1.setDocRef(1202L);
		try {
			assertNotNull(testSubject.parseDocument(doc1, null));
		} catch (ParsingException e) {
			// catching exception
		}
	}

	@Test
	public void testEnforceFilesIntoFolderStore() throws PersistenceException, IOException {
		folderDao.createPath(folderDao.findById(Folder.ROOTID), "/Default/test", true, null);

		DocumentHistory transaction = new DocumentHistory();
		User user = userDao.findByUsername("admin");
		transaction.setUser(user);

		Folder folder = folderDao.createPath(folderDao.findById(Folder.ROOTID), "/Default/test/subfolder", true, null);
		assertNull(folder.getStore());

		folder = folderDao.findByPathExtended("/Default/test", 1L);
		folderDao.initialize(folder);
		folder.setStore(2);
		folderDao.store(folder);
		testSubject.enforceFilesIntoFolderStore(folder.getId(), transaction);

		folder = folderDao.findByPathExtended("/Default/test", 1L);
		folderDao.initialize(folder);
		assertEquals(1, folder.getStores().size());
		assertEquals(Integer.valueOf(2), folder.getStore());

		folder = folderDao.findByPathExtended("/Default/test/subfolder", 1L);
		folderDao.initialize(folder);
		assertNull(folder.getStore());

		Document doc = docDao.findById(1);
		testSubject.moveToFolder(doc, folder, transaction);

		String storeRoot = Context.get().getProperties().getProperty("store.1.dir");
		String store2Root = Context.get().getProperties().getProperty("store.2.dir");

		assertTrue(new File(storeRoot + "/1/doc/" + doc.getFileVersion()).exists());
		FileUtil.delete(new File(store2Root + "/1/doc/"));

		transaction = new DocumentHistory();
		transaction.setUser(user);
		testSubject.enforceFilesIntoFolderStore(folder.getId(), transaction);

		assertTrue(new File(store2Root + "/1/doc/" + doc.getFileVersion()).exists());

		// rootId is null
		try {
			testSubject.enforceFilesIntoFolderStore(9999, transaction);
		} catch (PersistenceException e) {
			// catching the exception
		}

		// folderRef is null
		folder = folderDao.findById(1501);
		folderDao.initialize(folder);
		testSubject.enforceFilesIntoFolderStore(folder.getId(), transaction);
		folder.setStore(1);
	}

	@Test
	public void testRename() throws PersistenceException {
		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		docDao.store(doc);
		assertEquals("pippo.pdf", doc.getFileName());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		testSubject.rename(doc.getId(), "archimede.pdf", transaction);

		doc = docDao.findById(1);
		assertEquals("archimede.pdf", doc.getFileName());
	}

	@Test
	public void testReindex() throws PersistenceException, ParsingException {
		Document doc = docDao.findById(1);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());
		docDao.initialize(doc);
		doc.setIndexingStatus(0);
		docDao.store(doc);
		doc = docDao.findById(1);
		assertEquals(IndexingStatus.TO_INDEX, doc.getIndexed());

		Folder folder = folderDao.createPath(folderDao.findById(Folder.ROOTID), "/Default/test", true, null);

		// Create an alias
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		Document alias = testSubject.createAlias(doc, folder, null, transaction);

		transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		testSubject.index(doc.getId(), null, transaction);

		doc = docDao.findById(1);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());

		doc = docDao.findById(1);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());
		docDao.initialize(doc);
		doc.setIndexingStatus(0);
		docDao.store(doc);
		doc = docDao.findById(1);
		assertEquals(IndexingStatus.TO_INDEX, doc.getIndexed());

		transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		testSubject.index(alias.getId(), null, transaction);

		doc = docDao.findById(1);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());

		try {
			transaction = new DocumentHistory();
			transaction.setUser(userDao.findByUsername("admin"));
			testSubject.index(99999, null, transaction);
		} catch (IllegalArgumentException e) {
			// catching exception
		}
	}

	@Test
	public void testMakeImmutable() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		testSubject.makeImmutable(doc.getId(), transaction);
		doc = docDao.findById(1);
		assertEquals(1, doc.getImmutable());
		doc.setFileName("ciccio");
		docDao.initialize(doc);
		docDao.store(doc);
	}

	@Test
	public void testLock() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		testSubject.unlock(1L, transaction);
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		transaction.setComment("pippo_reason");
		transaction.setEvent((DocumentEvent) null);
		testSubject.lock(doc.getId(), DocumentStatus.LOCKED, transaction);
		doc = docDao.findById(1);
		assertEquals(2, doc.getStatus().ordinal());
		assertEquals(1L, doc.getLockUserId().longValue());

		// double lock with same user just to check that no exceptions are
		// raised
		testSubject.lock(doc.getId(), DocumentStatus.LOCKED, transaction);
		testSubject.lock(doc.getId(), DocumentStatus.LOCKED, transaction);

		// Now try to lock with an other user
		transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("boss"));

		boolean exceptionHappened = false;
		try {
			testSubject.lock(doc.getId(), DocumentStatus.LOCKED, transaction);
		} catch (PersistenceException e) {
			exceptionHappened = true;
			assertTrue(e.getMessage().contains("is already locked by user"));
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testUnLock() throws PersistenceException {
		User adminUser = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(adminUser);
		transaction.setNotified(0);

		// Already locked by same user
		try {
			testSubject.lock(1L, DocumentStatus.LOCKED, transaction);
			fail("An exception should have been raised here");
		} catch (PersistenceException e) {
			// All ok
		}

		Document doc = docDao.findById(1);
		assertEquals(1, doc.getStatus().ordinal());
		assertEquals(1L, doc.getLockUserId().longValue());

		transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("boss"));
		transaction.setNotified(0);

		// Locked by a different user
		try {
			testSubject.unlock(doc.getId(), transaction);
			fail("An exception should have been raised here");
		} catch (PersistenceException e) {
			// All ok
		}

		doc = docDao.findById(1);
		assertEquals(1, doc.getStatus().ordinal());
		assertEquals(1L, doc.getLockUserId().longValue());

		transaction = new DocumentHistory();
		transaction.setUser(adminUser);
		transaction.setNotified(0);
		testSubject.unlock(doc.getId(), transaction);

		doc = docDao.findById(1);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		assertNull(doc.getLockUserId());

		// Already unlocked
		testSubject.unlock(doc.getId(), transaction);
		transaction.setUser(userDao.findByUsername("boss"));
		doc = docDao.findById(1);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		assertNull(doc.getLockUserId());
	}

	@Test
	public void testMerge() throws PersistenceException, IOException {
		Document doc1 = docDao.findById(1);
		assertNotNull(doc1);
		docDao.initialize(doc1);
		assertEquals(55, testSubject.countPages(doc1));

		Document doc3 = docDao.findById(3);
		assertNotNull(doc3);
		docDao.initialize(doc3);
		assertEquals(1, testSubject.countPages(doc3));

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(userDao.findByUsername("admin"));
		Document mergedDoc = testSubject.merge(Arrays.asList(doc1, doc3), 1200L, "merged.pdf", transaction)
				.getDocument();
		assertNotNull(mergedDoc);

		mergedDoc = docDao.findById(mergedDoc.getId());
		assertNotNull(mergedDoc);
		docDao.initialize(mergedDoc);

		assertEquals(56, testSubject.countPages(mergedDoc));

		// Non-admin user
		Document doc8 = docDao.findById(8L);
		assertNotNull(doc8);
		docDao.initialize(doc8);
		assertNotNull(doc8);

		User testUser = userDao.findById(4L);
		userDao.initialize(testUser);
		transaction = new DocumentHistory();
		transaction.setUser(testUser);
		testSubject.merge(List.of(doc8, doc3), 1200L, "merged.pdf", transaction);
	}

	@Test
	public void testStoreVersionAsync() throws PersistenceException, InterruptedException, ExecutionException {
		// A new document will have ID=101 so we prepare a fake document with
		// that ID and create a version.
		Document doc = docDao.findById(1);
		docDao.initialize(doc);

		User user = userDao.findByUsername("admin");
		Version version = Version.create(doc, user, null, DocumentEvent.STORED, false);

		// Prepare a separate thread that creates the document
		Thread createDoc = new Thread() {

			@Override
			public void run() {
				try {
					Document doc = docDao.findById(1);
					doc = new Document(doc);
					DocumentHistory transaction = new DocumentHistory();
					transaction.setFolderId(103L);
					transaction.setUser(user);
					transaction.setDocId(doc.getId());
					transaction.setUserId(1L);
					transaction.setNotified(0);
					transaction.setComment("pippo_reason");
					doc.setCustomId("xxxxxxxxxx");
					doc.setId(0L);
					docDao.store(doc, transaction);
				} catch (Exception e) {
					// Nothing to do
				}
			}
		};
		createDoc.start();

		// This starts a new thread waiting for the referenced document to be
		// written. This fails some times because the referenced document is
		// not already available.
		DocumentManager docMan = (DocumentManager) testSubject;
		doc = docMan.storeVersionAsync(version, doc).get();

		assertEquals(doc.getId(), version.getDocId());
		assertNotNull(docDao.findById(version.getDocId()));
	}

	@Test
	public void testCreate()
			throws PersistenceException, FileNotFoundException, InterruptedException, ExecutionException {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);
		doc = new Document(doc);
		doc.setId(0);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		doc.setCustomId("xxxxxxxxxx");

		DocumentFuture result = testSubject.create(new FileInputStream("pom.xml"), doc, transaction);
		Document newDoc = result.get();
		assertEquals("1.0", newDoc.getVersion());
		assertEquals("1.0", newDoc.getFileVersion());

		Version ver = verDao.findByVersion(newDoc.getId(), newDoc.getVersion());
		assertNotNull(ver);

		newDoc = docDao.findById(newDoc.getId());
		assertEquals(newDoc.getFileName(), doc.getFileName());

		// Transaction is null
		try {
			result = testSubject.create(new FileInputStream("pom.xml"), doc, null);
			newDoc = result.getDocument();
		} catch (IllegalArgumentException e) {
			assertEquals(null, newDoc.getTransactionId());
		}

		// DocumentVO is null
		doc = docDao.findById(9999L);
		try {
			result = testSubject.create(new FileInputStream("pom.xml"), doc, transaction);
			newDoc = result.getDocument();
		} catch (IllegalArgumentException e) {
			assertEquals(null, doc);
		}

		// Overridden method - transaction is null
		File file = new File("target/testFile");
		doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);
		try {
			result = testSubject.create(file, doc, null);
			newDoc = result.getDocument();
		} catch (IllegalArgumentException e) {
			// catch exception
		}

		File emptyFile = new File("emptyFile");
		// File with 0 length
		try {
			result = testSubject.create(emptyFile, doc, transaction);
			newDoc = result.getDocument();
		} catch (IllegalArgumentException e) {
			assertEquals(0, emptyFile.length());
		}

		// File with content
		doc.setTemplateId(1L);
		try (FileWriter writer = new FileWriter(file)) {
			writer.write("Hello, world!");
		} catch (IOException e) {
			e.printStackTrace();
		}

		result = testSubject.create(file, doc, transaction);
		newDoc = result.getDocument();
		assertNotNull(newDoc);
	}

	@Test
	public void testCreateAlias() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(6L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		Folder newFolder = folderDao.findById(6);
		folderDao.initialize(newFolder);

		Document alias = testSubject.createAlias(doc, newFolder, null, transaction);

		assertNotSame(doc.getId(), alias.getId());
		assertEquals(newFolder, alias.getFolder());
		assertEquals("pippo(1).pdf", alias.getFileName());

		testSubject.createAlias(doc, newFolder, "aliasTypeTest", transaction);

		// non-existent folder
		Folder folder = folderDao.findById(9999);
		try {
			testSubject.createAlias(doc, folder, null, transaction);
		} catch (IllegalArgumentException e) {
			assertEquals(null, folder);
		}

		// non-existent document
		doc = docDao.findById(9999);
		try {
			testSubject.createAlias(doc, newFolder, null, transaction);
		} catch (IllegalArgumentException e) {
			assertEquals(null, doc);
		}
	}

	@Test
	public void testReplaceAlias() throws PersistenceException, IOException {
		User user = userDao.findByUsername("admin");
		Document originalDoc = docDao.findById(1);
		assertNotNull(originalDoc);
		docDao.initialize(originalDoc);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(6L);
		transaction.setUser(user);
		transaction.setDocId(originalDoc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		Folder newFolder = folderDao.createPath(folderDao.findById(6), "/test", false, null);

		Document alias = testSubject.createAlias(originalDoc, newFolder, null, transaction);
		assertNotNull(alias);

		try {
			store.setUseDummyFile(true);
			Document newDoc = testSubject.replaceAlias(alias.getId(), new DocumentHistory(transaction)).getDocument();

			assertNotNull(newDoc);
			assertEquals(originalDoc.getFileName(), newDoc.getFileName());
			assertNotSame(originalDoc.getFolder(), newDoc.getFolder());
			alias = docDao.findById(alias.getId());
			assertNull(alias);
		} finally {
			store.setUseDummyFile(false);
		}

		// Alias is null
		try {
			testSubject.replaceAlias(9999, transaction);
		} catch (PersistenceException e) {
			// catch exception
		}

		user = userDao.findById(2L);

		transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setUserId(2L);

		Set<FolderAccessControlEntry> acl = new HashSet<>();
		FolderAccessControlEntry ace = new FolderAccessControlEntry();
		ace.setGroupId(2L);
		ace.setRead(1);
		ace.setWrite(0);
		acl.add(ace);

		Folder folder1 = new Folder();
		folder1.setParentId(4);
		folder1.setName("folderTest");
		folder1.setAccessControlList(acl);
		folderDao.store(folder1);

		Document doc1 = new Document();
		doc1.setFolder(folder1);
		doc1.setFileName("doc1_name");
		doc1.setVersion("1.0");
		docDao.store(doc1);
		assertNotNull(doc1);

		Document doc2 = new Document();
		doc2.setFolder(folder1);
		doc2.setFileName("doc2_name");
		doc2.setDocRef(3L);
		doc2.setVersion("1.0");
		docDao.store(doc2);
		assertNotNull(doc2);

		String resource = store.getResourceName(doc2.getId(), doc2.getFileVersion(), null);
		store2.store(ResourceUtil.getInputStream("allowed-commands.txt"), 3L, resource);

		try {
			testSubject.replaceAlias(doc2.getId(), transaction);
		} catch (PersistenceException e) {
			// catch exception
		}
	}

	@Test
	public void testCheckin() throws PersistenceException, IOException, InterruptedException, ExecutionException {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		testSubject.checkout(1L, transaction);

		Document doc = docDao.findById(1L);
		assertNotNull(doc);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());
		docDao.initialize(doc);

		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());
		assertEquals("1.0", doc.getFileVersion());
		assertNotNull(documentNoteDao.findById(2L));

		try (InputStream is = ResourceUtil.getInputStream("abel.eml")) {
			testSubject.checkin(1L, is, "pippo", true, null, new DocumentHistory(transaction)).get();
		}
		doc = docDao.findById(1L);

		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
		assertEquals("2.0", doc.getFileVersion());

		doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);

		assertEquals(IndexingStatus.TO_INDEX, doc.getIndexed());
		assertEquals(0, doc.getSigned());
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

		testSubject.checkout(1L, transaction);

		doc = docDao.findById(1L);
		docDao.initialize(doc);

		transaction.setComment("reason2");
		try (InputStream is = ResourceUtil.getInputStream("abel.eml")) {
			testSubject.checkin(1L, is, "pippo", true, doc, new DocumentHistory(transaction)).get();
		}

		doc = docDao.findById(1L);
		docDao.initialize(doc);
		assertEquals("reason2", doc.getComment());

		// Reproduce an error in the store
		transaction.setComment("reason3");
		store.setErrorOnStore(true);

		boolean exceptionHappened = false;
		try (InputStream is = ResourceUtil.getInputStream("abel.eml")) {
			testSubject.checkin(1L, is, "pippo", true, doc, new DocumentHistory(transaction)).get();
		} catch (PersistenceException e) {
			exceptionHappened = true;
			assertEquals("Cannot save the new version pippo (1) into the store", e.getMessage());
		}
		assertTrue(exceptionHappened);

		File file = new File("target/testFile");

		// immutable is not 0
		doc = docDao.findById(9);
		assertNotNull(doc);
		try {
			testSubject.checkin(9, file, "testFile", true, doc, new DocumentHistory(transaction)).get();
		} catch (PersistenceException e) {
			// All ok
		}

		// null filename
		try {
			testSubject.checkin(9, file, null, true, doc, new DocumentHistory(transaction)).get();
		} catch (IllegalArgumentException e) {
			// All ok
		}

		doc = docDao.findById(1);
		assertEquals(null, doc.getComment());

		transaction.setEvent((DocumentEvent) null);
		testSubject.checkout(1L, new DocumentHistory(transaction));
	}

	@Test
	public void testChangeIndexingStatus() throws PersistenceException {
		Document doc = docDao.findById(1L);
		assertNotNull(doc);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());
		testSubject.changeIndexingStatus(doc, IndexingStatus.SKIP);
		assertEquals(IndexingStatus.SKIP, doc.getIndexed());

		doc = docDao.findById(2L);
		assertNotNull(doc);
		assertEquals(IndexingStatus.TO_INDEX, doc.getIndexed());
		testSubject.changeIndexingStatus(doc, IndexingStatus.SKIP);
		assertEquals(IndexingStatus.SKIP, doc.getIndexed());

		Document docTest = new Document();
		docTest.setFileName("docTest");
		docTest.setFolder(folderDao.findById(Folder.DEFAULTWORKSPACEID));
		docDao.store(docTest);

		docTest = docDao.findById(docTest.getId());
		docDao.initialize(docTest);
		docTest.setIndexingStatus(IndexingStatus.SKIP);
		docTest.setStatus(DocumentStatus.LOCKED);
		testSubject.changeIndexingStatus(docTest, IndexingStatus.SKIP);
		assertEquals(IndexingStatus.SKIP, docTest.getIndexed());

		docTest = docDao.findById(docTest.getId());
		docDao.initialize(docTest);
		docTest.setIndexingStatus(IndexingStatus.TO_INDEX);
		docTest.setStatus(DocumentStatus.UNLOCKED);
		testSubject.changeIndexingStatus(docTest, IndexingStatus.TO_INDEX);
		assertEquals(IndexingStatus.TO_INDEX, docTest.getIndexed());

		docTest = docDao.findById(docTest.getId());
		docDao.initialize(docTest);
		docTest.setIndexingStatus(IndexingStatus.TO_INDEX_METADATA);
		docTest.setStatus(DocumentStatus.ARCHIVED);
		testSubject.changeIndexingStatus(docTest, IndexingStatus.TO_INDEX_METADATA);
		assertEquals(IndexingStatus.TO_INDEX_METADATA, docTest.getIndexed());
	}

	@Test
	public void testDeleteVersion() throws PersistenceException {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("1234");
		transaction.setUser(userDao.findByUsername("admin"));

		assertNotNull(verDao.findById(11L));
		testSubject.deleteVersion(11L, transaction);
		assertNull(verDao.findById(11L));

		assertNotNull(verDao.findById(13L));
		testSubject.deleteVersion(13L, null);
		assertNull(verDao.findById(13L));
	}

	@Test
	public void testDestroyDocument() throws PersistenceException, PermissionException {
		FolderHistory transaction = new FolderHistory();
		transaction.setSessionId("1234");
		transaction.setUser(userDao.findByUsername("admin"));

		Document doc = docDao.findById(1L);
		assertNotNull(doc);
		assertTrue(store.exists(1L, store.getResourceName(doc, null, null)));

		testSubject.destroyDocument(1L, transaction);
		assertNotNull(docDao.findById(1L));

		// We should check that the file does not exist anymore in the store
		// but quite ofter it does not get immediately deleted by the JVM
		// so we do not test in order to complete all the tests without errors
	}

	@Test
	public void testArchiveDocuments() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("1234");
		transaction.setUser(user);

		testSubject.archiveDocuments(Set.of(1L), transaction);

		Document doc = docDao.findById(1L);
		assertEquals(DocumentStatus.ARCHIVED, doc.getStatus());

		// Non-existing user
		user = userDao.findById(9999);
		transaction = new DocumentHistory();
		transaction.setSessionId("1234");
		transaction.setUser(user);

		try {
			testSubject.archiveDocuments(Set.of(2L), transaction);
		} catch (IllegalArgumentException e) {
			assertEquals(null, transaction.getUser());
		}

		// Non-admin user
		user = new User();
		user.setPassword("admin");
		user.setName("testUser");
		userDao.store(user);
		transaction = new DocumentHistory();
		transaction.setSessionId("1234");
		transaction.setUser(user);
		testSubject.archiveDocuments(Set.of(1L), transaction);
		assertEquals(DocumentStatus.ARCHIVED, doc.getStatus());
	}

	@Test
	public void testUnprotect() throws PersistenceException {
		Document doc = docDao.findById(3L);
		assertNull(doc.getPassword());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		docDao.setPassword(3L, "test", history);

		Session session = SessionManager.get().newSession("admin", "admin", (Client) null);

		assertTrue(testSubject.unprotect(session.getSid(), 3L, "test"));
		assertFalse(testSubject.unprotect(session.getSid(), 3L, "test2"));
		assertTrue(testSubject.unprotect(session.getSid(), 3L, "test"));
	}

	@Test
	public void testReplaceFile() throws PersistenceException, IOException {
		Document doc = docDao.findById(3L);
		assertNotNull(doc);
		assertEquals("1.3", doc.getVersion());
		assertEquals("1.3", doc.getFileVersion());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");

		history.setSession(SessionManager.get().newSession("admin", "admin", (Client) null));

		try (InputStream is = ResourceUtil.getInputStream("abel.eml")) {
			testSubject.replaceFile(doc.getId(), "1.3", is, history);
		}

		Store str = Context.get(Store.class);

		doc = docDao.findById(3L);
		assertNotNull(doc);
		assertEquals("1.3", doc.getVersion());
		assertEquals("1.3", doc.getFileVersion());
		assertTrue(str.getString(doc.getId(), str.getResourceName(doc, null, null)).contains("invoice calculation"));

		try (InputStream is = ResourceUtil.getInputStream("abel.eml")) {
			testSubject.replaceFile(doc.getId(), "1.3", is, null);
		} catch (IllegalArgumentException e) {
			// catch exception
		}

		File newFile = new File("newFile");

		// null transaction
		try {
			testSubject.replaceFile(3L, "1.3", newFile, null);
		} catch (IllegalArgumentException e) {
			// catch exception
		}

		// non-existent user
		DocumentHistory history2 = new DocumentHistory();
		history.setUserId(9999L);
		history.setUsername("nonExistent");
		try {
			testSubject.replaceFile(3L, "1.3", newFile, history2);
		} catch (IllegalArgumentException e) {
			// catch exception
		}

	}

	@Test
	public void testPromoteVersion() throws PersistenceException, IOException {
		Document doc = docDao.findById(3L);
		assertNotNull(doc);
		assertEquals("1.3", doc.getVersion());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		history.setSession(SessionManager.get().newSession("admin", "admin", (Client) null));

		doc = docDao.findById(3L);
		assertNotNull(doc);
		assertEquals("1.3", doc.getVersion());

		// Use a dummy file with correct size
		File dummyFile = new File("target/dummy.pdf");
		try (RandomAccessFile raf = new RandomAccessFile(dummyFile, "rw");) {
			raf.setLength(3116);
		}

		try {
			store.setDummyFile(dummyFile);
			store.setUseDummyFile(true);

			testSubject.promoteVersion(3L, "1.3", history);
		} finally {
			store.setUseDummyFile(false);
			FileUtil.delete(dummyFile);
		}

		// Version with templateIs and tgs
		Document doc8 = docDao.findById(8L);
		docDao.initialize(doc8);
		assertNotNull(doc8);
		assertEquals("1.0", doc8.getVersion());
		doc8.setStatus(DocumentStatus.UNLOCKED);
		docDao.store(doc8);
		testSubject.promoteVersion(doc8.getId(), "1.0", history);
	}

	@Test
	public void testArchiveFolder() throws PersistenceException {
		Document doc = docDao.findById(1L);
		assertNotNull(doc);
		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		history.setSession(SessionManager.get().newSession("admin", "admin", (Client) null));
		testSubject.unlock(1L, history);
		doc = docDao.findById(1L);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

		assertEquals(6, testSubject.archiveFolder(6L, history));

		doc = docDao.findById(1L);
		assertNotNull(doc);
		assertEquals(DocumentStatus.ARCHIVED, doc.getStatus());
	}

	@Test
	public void testFailedStoreCreate() throws PersistenceException, FileNotFoundException {
		Document doc = new Document();
		doc.setFileName("failed.txt");
		doc.setFolder(folderDao.findById(Folder.DEFAULTWORKSPACEID));

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		history.setSession(SessionManager.get().newSession("admin", "admin", (Client) null));

		FileInputStream fis = new FileInputStream("pom.xml");

		boolean exceptionHappened = false;
		try {
			store.setErrorOnStore(true);
			testSubject.create(fis, doc, history);
		} catch (Exception e) {
			exceptionHappened = true;
		} finally {
			store.setErrorOnStore(false);
		}
		assertTrue(exceptionHappened);

		// Now check that the document was deleted
		assertNull(docDao.findById(doc.getId()));
	}

	@Test
	public void testFailedStoreCheckin() throws PersistenceException {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		testSubject.checkout(1L, transaction);

		File file = new File("pom.xml");

		Document doc = docDao.findById(1L);
		assertNotNull(doc);
		assertEquals(IndexingStatus.INDEXED, doc.getIndexed());
		docDao.initialize(doc);

		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());
		assertEquals("1.0", doc.getFileVersion());
		assertNotNull(documentNoteDao.findById(2L));

		try {
			store.setErrorOnStore(true);
			testSubject.checkin(1L, file, "pippo", true, null, transaction);
			fail("an exception should have been raised at this point");
		} catch (Exception e) {
			// Noting to do
		} finally {
			store.setErrorOnStore(false);
		}

		doc = docDao.findById(1L);

		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());
		assertEquals("1.0", doc.getFileVersion());
	}
}