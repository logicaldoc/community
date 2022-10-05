package com.logicaldoc.core.document;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.util.Context;

import junit.framework.Assert;

/**
 * Test case for <code>DocumentManagerImpl</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public class DocumentManagerImplTest extends AbstractCoreTCase {

	private DocumentDAO docDao;

	private VersionDAO verDao;

	private UserDAO userDao;

	private FolderDAO folderDao;

	private DocumentNoteDAO documentNoteDao;

	// Instance under test
	private DocumentManager documentManager;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		docDao = (DocumentDAO) context.getBean("DocumentDAO");
		verDao = (VersionDAO) context.getBean("VersionDAO");
		userDao = (UserDAO) context.getBean("UserDAO");
		folderDao = (FolderDAO) context.getBean("FolderDAO");
		documentNoteDao = (DocumentNoteDAO) context.getBean("DocumentNoteDAO");

		// Make sure that this is a DocumentManagerImpl instance
		documentManager = (DocumentManager) context.getBean("DocumentManager");
	}

	@Test
	public void testUpdate() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());

		Document newDoc = docDao.findById(2);
		Assert.assertNotNull(newDoc);
		Assert.assertEquals("pluto", newDoc.getFileName());

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

		documentManager.update(doc, newDoc, transaction);

		Assert.assertEquals("pluto(1)", doc.getFileName());
	}

	@Test
	public void testCreateDownloadTicket() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);

		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(user);
		transaction.setUserId(1L);
		transaction.setNotified(0);

		Ticket t = documentManager.createDownloadTicket(1L, null, null, null, null, null, transaction);
		Assert.assertNotNull(t.getUrl());

		synchronized (this) {
			try {
				wait(1000);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}

		t = documentManager.createDownloadTicket(1L, null, 2, null, null, null, transaction);
		Assert.assertNotNull(t.getUrl());
	}

	@Test
	public void testCopyToFolder() throws Exception {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Folder folder = doc.getFolder();
		Assert.assertEquals(6, folder.getId());

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

		DummyStorer storer = (DummyStorer) Context.get().getBean(Storer.class);
		try {
			storer.setUseDummyFile(true);
			Document newDoc = documentManager.copyToFolder(doc, newFolder, transaction);
			Assert.assertNotSame(doc.getId(), newDoc.getId());
			Assert.assertEquals(newFolder, newDoc.getFolder());
		} finally {
			storer.setUseDummyFile(false);
		}
	}

	@Test
	public void testMoveToFolder() throws Exception {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Folder folder = doc.getFolder();
		Assert.assertEquals(6, folder.getId());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(6L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		Folder newFolder = folderDao.findById(6);
		docDao.initialize(doc);
		documentManager.moveToFolder(doc, newFolder, transaction);
		Assert.assertSame(1L, doc.getId());
		Assert.assertEquals(newFolder, doc.getFolder());
	}

	@Test
	public void testMakeImmutable() throws Exception {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(doc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");
		documentManager.makeImmutable(doc.getId(), transaction);
		doc = docDao.findById(1);
		Assert.assertEquals(1, doc.getImmutable());
		doc.setFileName("ciccio");
		docDao.initialize(doc);
		docDao.store(doc);
	}

	@Test
	public void testLock() throws Exception {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		documentManager.unlock(1L, transaction);
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		transaction.setComment("pippo_reason");
		documentManager.lock(doc.getId(), 2, transaction);
		doc = docDao.findById(1);
		Assert.assertEquals(2, doc.getStatus());
		Assert.assertEquals(1L, doc.getLockUserId().longValue());
	}

	@Test
	public void testCreate() throws Exception {
		
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
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

		Document newDoc = documentManager.create(new FileInputStream("pom.xml"), doc, transaction);

		Assert.assertEquals("1.0",newDoc.getVersion());
		Assert.assertEquals("1.0",newDoc.getFileVersion());
		
		Version ver=verDao.findByVersion(newDoc.getId(), newDoc.getVersion());
		Assert.assertNotNull(ver);
		
		newDoc = docDao.findById(newDoc.getId());
		Assert.assertEquals(newDoc.getFileName(), doc.getFileName());
	}

	@Test
	public void testCreateAlias() throws Exception {
		User user = userDao.findByUsername("admin");
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
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

		Document alias = documentManager.createAlias(doc, newFolder, null, transaction);

		Assert.assertNotSame(doc.getId(), alias.getId());
		Assert.assertEquals(newFolder, alias.getFolder());
		Assert.assertEquals("pippo(1)", alias.getFileName());
	}

	@Test
	public void testReplaceAlias() throws Exception {
		User user = userDao.findByUsername("admin");
		Document originalDoc = docDao.findById(1);
		Assert.assertNotNull(originalDoc);
		docDao.initialize(originalDoc);
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(6L);
		transaction.setUser(user);
		transaction.setDocId(originalDoc.getId());
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		Folder newFolder = folderDao.createPath(folderDao.findById(6), "/test", false, null);

		Document alias = documentManager.createAlias(originalDoc, newFolder, null, transaction);
		Assert.assertNotNull(alias);

		DummyStorer storer = (DummyStorer) Context.get().getBean(Storer.class);
		try {
			storer.setUseDummyFile(true);
			Document newDoc = documentManager.replaceAlias(alias.getId(), (DocumentHistory) transaction.clone());

			Assert.assertNotNull(newDoc);
			Assert.assertEquals(originalDoc.getFileName(), newDoc.getFileName());
			Assert.assertNotSame(originalDoc.getFolder(), newDoc.getFolder());
			alias = docDao.findById(alias.getId());
			Assert.assertNull(alias);
		} finally {
			storer.setUseDummyFile(false);
		}
	}

	@Test
	public void testCheckin() throws Exception {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		documentManager.checkout(1L, transaction);

		File file = new File("pom.xml");

		Document doc = docDao.findById(1L);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.INDEX_INDEXED, doc.getIndexed());
		docDao.initialize(doc);

		Assert.assertEquals(Document.DOC_CHECKED_OUT, doc.getStatus());
		Assert.assertEquals("1.0", doc.getFileVersion());
		Assert.assertNotNull(documentNoteDao.findById(2L));

		documentManager.checkin(1L, file, "pippo", true, null, transaction);
		doc = docDao.findById(1L);

		Assert.assertEquals(Document.DOC_UNLOCKED, doc.getStatus());
		Assert.assertEquals("2.0", doc.getFileVersion());

		doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		docDao.initialize(doc);

		Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX, doc.getIndexed());
		Assert.assertEquals(0, doc.getSigned());
		Assert.assertEquals(Document.DOC_UNLOCKED, doc.getStatus());

		documentManager.checkout(1L, transaction);
		doc = docDao.findById(1);
		docDao.initialize(doc);
		Assert.assertEquals(Document.DOC_CHECKED_OUT, doc.getStatus());

		transaction.setComment("reason2");
		documentManager.checkin(1L, file, "pippo", true, doc, transaction);
		doc = docDao.findById(1);
		Assert.assertEquals("reason2", doc.getComment());
	}

	@Test
	public void testChangeIndexingStatus() throws PersistenceException {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.INDEX_INDEXED, doc.getIndexed());
		documentManager.changeIndexingStatus(doc, AbstractDocument.INDEX_SKIP);
		Assert.assertEquals(AbstractDocument.INDEX_SKIP, doc.getIndexed());

		doc = docDao.findById(2);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX, doc.getIndexed());
		documentManager.changeIndexingStatus(doc, AbstractDocument.INDEX_SKIP);
		Assert.assertEquals(AbstractDocument.INDEX_SKIP, doc.getIndexed());
	}

	@Test
	public void testDeleteVersion() throws Exception {
		Assert.assertNotNull(verDao.findById(11L));
		documentManager.deleteVersion(11L, null);
		Assert.assertNull(verDao.findById(11L));

		Assert.assertNotNull(verDao.findById(13L));
		documentManager.deleteVersion(13L, null);
		Assert.assertNull(verDao.findById(13L));
	}

	@Test
	public void testArchiveDocuments() throws Exception {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("1234");
		transaction.setUser(user);

		documentManager.archiveDocuments(new long[] { 1L }, transaction);

		Document doc = docDao.findById(1L);
		Assert.assertEquals(AbstractDocument.DOC_ARCHIVED, doc.getStatus());
	}

	@Test
	public void testUnprotect() throws Exception {
		Document doc = docDao.findById(3L);
		Assert.assertNull(doc.getPassword());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		docDao.setPassword(3L, "test", history);

		Session session = SessionManager.get().newSession("admin", "admin", null);

		Assert.assertTrue(documentManager.unprotect(session.getSid(), 3L, "test"));
		Assert.assertFalse(documentManager.unprotect(session.getSid(), 3L, "test2"));
		Assert.assertTrue(documentManager.unprotect(session.getSid(), 3L, "test"));
	}

	@Test
	public void testPromoteVersion() throws Exception {
		Document doc = docDao.findById(3L);
		Assert.assertNotNull(doc);
		Assert.assertEquals("1.3", doc.getVersion());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		history.setSession(SessionManager.get().newSession("admin", "admin", null));

		DummyStorer storer = (DummyStorer) Context.get().getBean(Storer.class);
		try {
			storer.setUseDummyFile(true);
			documentManager.promoteVersion(3L, "1.3", history);
		} finally {
			storer.setUseDummyFile(false);
		}
		doc = docDao.findById(3L);
		Assert.assertNotNull(doc);
		Assert.assertEquals("1.4", doc.getVersion());
		Assert.assertEquals("pippo.pdf", doc.getFileName());
	}

	@Test
	public void testFailedStoreCreate() throws Exception {
		Document doc = new Document();
		doc.setFileName("failed.txt");
		doc.setFolder(folderDao.findById(Folder.DEFAULTWORKSPACEID));

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		history.setSession(SessionManager.get().newSession("admin", "admin", null));

		DummyStorer storer = (DummyStorer) Context.get().getBean(Storer.class);
		/*try {
			storer.setRaiseError(true);
			documentManager.create(new FileInputStream("pom.xml"), doc, history);
			Assert.fail("an exception should have been raised at this point");
		} catch (Throwable e) {

		} finally {
			storer.setRaiseError(false);
		}*/
		

		FileInputStream fis = new FileInputStream("pom.xml");
		
		try {
			storer.setRaiseError(true);
			documentManager.create(fis, doc, history);
			Assert.fail("Expected an IOException to be thrown");
		} catch (Exception e) {
		} finally {
			storer.setRaiseError(false);
		}
		

		// Now check that the document was deleted
		Assert.assertTrue(doc.getId() != 0L);
		Assert.assertNull(docDao.findById(doc.getId()));
	}

	@Test
	public void testFailedStoreCheckin() throws Exception {
		User user = userDao.findByUsername("admin");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setUser(user);
		transaction.setDocId(1L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		transaction.setComment("pippo_reason");

		documentManager.checkout(1L, transaction);

		File file = new File("pom.xml");

		Document doc = docDao.findById(1L);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.INDEX_INDEXED, doc.getIndexed());
		docDao.initialize(doc);

		Assert.assertEquals(Document.DOC_CHECKED_OUT, doc.getStatus());
		Assert.assertEquals("1.0", doc.getFileVersion());
		Assert.assertNotNull(documentNoteDao.findById(2L));

		DummyStorer storer = (DummyStorer) Context.get().getBean(Storer.class);
		/*
		 * try { storer.setRaiseError(true); documentManager.checkin(1L, file, "pippo",
		 * true, null, transaction);
		 * Assert.fail("an exception should have been raised at this point"); } catch
		 * (Throwable e) {
		 * 
		 * } finally { storer.setRaiseError(false); }
		 */
		try {
			storer.setRaiseError(true); 
			documentManager.checkin(1L, file, "pippo", true, null, transaction);
			Assert.fail("an exception should have been raised at this point");
		} catch (Exception e) {
		} finally { 
			storer.setRaiseError(false); 
		}

		 
		doc = docDao.findById(1L);

		Assert.assertEquals(Document.DOC_CHECKED_OUT, doc.getStatus());
		Assert.assertEquals("1.0", doc.getFileVersion());
	}
}