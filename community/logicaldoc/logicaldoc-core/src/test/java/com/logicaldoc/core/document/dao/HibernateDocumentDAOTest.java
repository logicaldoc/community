package com.logicaldoc.core.document.dao;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;

import com.ibm.icu.util.Calendar;
import com.ibm.icu.util.GregorianCalendar;
import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.TagsProcessor;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.lock.LockManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * Test case for <code>HibernateDocumentDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentDAOTest extends AbstractCoreTCase {

	// Instance under test
	private DocumentDAO dao;

	private FolderDAO folderDao;

	private LockManager lockManager;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		dao = (DocumentDAO) context.getBean("DocumentDAO");
		folderDao = (FolderDAO) context.getBean("FolderDAO");
		lockManager = (LockManager) context.getBean("LockManager");
	}

	@Test
	public void testDelete() {
		// Create the document history event
		History transaction = new History();
		transaction.setSessionId("123");
		transaction.setEvent(DocumentEvent.DELETED.toString());
		transaction.setComment("");
		transaction.setUser(new User());

		Assert.assertTrue(dao.delete(1, transaction));
		Document doc = dao.findById(1);
		Assert.assertNull(doc);
	}

	@Test
	public void testArchive() {
		// Create the document history event
		History transaction = new History();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		Assert.assertTrue(dao.archive(1, transaction));
		Document doc = dao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.DOC_ARCHIVED, doc.getStatus());
	}

	@Test
	public void testFindArchivedByFolder() {
		// Create the document history event
		History transaction = new History();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		Assert.assertTrue(dao.archive(1, transaction));
		Document doc = dao.findById(1);

		List<Document> docs = dao.findArchivedByFolder(doc.getFolder().getId());
		Assert.assertEquals(1, docs.size());
		Assert.assertEquals(doc, docs.get(0));
	}

	@Test
	public void testUnArchive() {
		// Create the document history event
		History transaction = new History();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		Assert.assertTrue(dao.archive(1, transaction));
		Document doc = dao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.DOC_ARCHIVED, doc.getStatus());

		dao.unarchive(1, transaction);
		doc = dao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
	}

	@Test
	public void testFindAll() {
		Collection<Document> documents = dao.findAll();
		Assert.assertNotNull(documents);
		Assert.assertEquals(3, documents.size());

		Assert.assertEquals(3, dao.findByWhere("1=1", null, null).size());
		Assert.assertEquals(1, dao.findByWhere("1=1", null, 1).size());
	}

	@Test
	public void testFindById() {
		Document doc = dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		Assert.assertEquals(1, doc.getId());
		Assert.assertEquals("pippo", doc.getFileName());
		Assert.assertNotNull(doc.getFolder());
		Assert.assertEquals(6, doc.getFolder().getId());

		// Try with unexisting document
		doc = dao.findById(99);
		Assert.assertNull(doc);
	}

	@Test
	public void testFindByCustomId() {
		Document doc = dao.findByCustomId("a", Tenant.DEFAULT_ID);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		Assert.assertEquals(1, doc.getId());
		Assert.assertEquals("pippo", doc.getFileName());
		Assert.assertNotNull(doc.getFolder());
		Assert.assertEquals(6, doc.getFolder().getId());

		// Try with unexisting document
		doc = dao.findByCustomId("xx", Tenant.DEFAULT_ID);
		Assert.assertNull(doc);

		// Try with unexisting tenant
		doc = dao.findByCustomId("a", 99L);
		Assert.assertNull(doc);
	}

	@Test
	public void testFindByUserId() {
		Collection<Long> ids = dao.findByUserId(3);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());
		Assert.assertTrue(ids.contains(new Long(2)));

		// Try with a user without documents
		ids = dao.findByUserId(2);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testFindDocIdByFolder() {
		Collection<Long> ids = dao.findDocIdByFolder(6, null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(3, ids.size());
		Assert.assertTrue(ids.contains(new Long(2)));

		ids = dao.findDocIdByFolder(1111, null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testFindByFolder() {
		Collection<Document> docs = dao.findByFolder(6, null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(3, docs.size());
		Assert.assertTrue(docs.contains(dao.findById(2)));

		docs = dao.findByFolder(1111, null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(0, docs.size());
	}

	@Test
	public void testFindIndexed() {
		List<Document> docs = dao.findByIndexed(1);
		Assert.assertNotNull(docs);
		Assert.assertEquals(2, docs.size());
		Assert.assertEquals(1, docs.get(0).getId());

		docs = dao.findByIndexed(0);
		Assert.assertNotNull(docs);
		// The document with is 2 has a docRef not null
		Assert.assertEquals(0, docs.size());
	}

	@Test
	public void testFindLastModifiedByUserId() {
		Collection<Document> coll = dao.findLastModifiedByUserId(1, 10);
		Assert.assertNotNull(coll);
		Assert.assertEquals(4, coll.size());

		coll = dao.findLastModifiedByUserId(3, 10);
		Assert.assertNotNull(coll);
		Assert.assertEquals(0, coll.size());
	}

	@Test
	public void testFindDocIdByTag() {
		Collection<Long> ids = dao.findDocIdByTag("abc");
		Assert.assertNotNull(ids);
		Assert.assertEquals(1, ids.size());
		Assert.assertEquals(new Long(1), ids.iterator().next());

		ids = dao.findDocIdByTag("xxx");
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testDeleteOrphaned() {
		folderDao.delete(6);
		Assert.assertNull(folderDao.findById(6));
		dao.deleteOrphaned(1);
		Document doc = dao.findById(1);
		Assert.assertTrue(doc == null || doc.getDeleted() == 1);
	}

	@Test
	public void testFindPublishedIds() throws IOException {
		GregorianCalendar cal = new GregorianCalendar();

		Document doc = new Document();
		Folder folder = folderDao.findById(Folder.ROOTID);
		doc.setFolder(folder);
		doc.setFileVersion("1.0");
		doc.setVersion("1.0");
		doc.setPublisher("admin");
		doc.setPublisherId(1);
		doc.setFileName("test.txt");

		// Prepare the document file for digest computation
		File docFile = new File("target");
		docFile = new File(docFile, "store");
		docFile = new File(docFile, doc.getFileVersion());
		FileUtils.forceMkdir(docFile.getParentFile());
		Writer out = new FileWriter(docFile);
		out.write("Questo file serve per fare il test del digest su un documento");
		out.flush();
		out.close();
		Assert.assertTrue(docFile.exists());

		dao.store(doc);

		Set<Long> fids = new HashSet<Long>();
		fids.add(Folder.ROOTID);

		Collection<Long> ids = dao.findPublishedIds(fids);
		Assert.assertTrue(ids.contains(doc.getId()));

		doc.setPublished(0);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		Assert.assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		Date pick = cal.getTime();
		doc.setPublished(1);
		doc.setStartPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		Assert.assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, -3);
		pick = cal.getTime();
		doc.setStartPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		Assert.assertTrue(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		pick = cal.getTime();
		doc.setStopPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		Assert.assertFalse(ids.contains(doc.getId()));
	}

	@Test
	public void testStore() throws IOException {
		Document doc = new Document();
		Folder folder = folderDao.findById(Folder.ROOTID);

		doc = dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);

		// Try to store it inside a folder with extended attributes
		folder = folderDao.findById(1202);
		doc.setFolder(folder);

		doc.setValue("object", "test");
		dao.store(doc);
		doc = dao.findById(doc.getId());
		dao.initialize(doc);

		// Check if the defaults were applied
		doc = dao.findById(1);
		dao.initialize(doc);
		Assert.assertEquals(1, doc.getTemplate().getId());
		Assert.assertEquals("test_val_1", doc.getValue("val1"));
	}

	@Test
	public void testFindTags() {
		TagsProcessor processor = (TagsProcessor) context.getBean("TagsProcessor");
		processor.run();

		Collection<String> tags = dao.findTags("a", 1L).keySet();
		Assert.assertNotNull(tags);
		Assert.assertEquals(2, tags.size());
		Assert.assertTrue(tags.contains("abc"));
		tags = dao.findTags("a", null).keySet();
		Assert.assertNotNull(tags);
		Assert.assertEquals(2, tags.size());
		Assert.assertTrue(tags.contains("abc"));

		tags = dao.findTags("a", 99L).keySet();
		Assert.assertNotNull(tags);
		Assert.assertEquals(0, tags.size());
	}

	@Test
	public void testFindAllTags() {
		TagsProcessor processor = (TagsProcessor) context.getBean("TagsProcessor");
		processor.run();

		Collection<String> tags = dao.findAllTags("a", 1L);
		Assert.assertNotNull(tags);
		Assert.assertEquals(2, tags.size());
		Assert.assertTrue(tags.contains("abc"));
		tags = dao.findAllTags(null, 1L);
		Assert.assertNotNull(tags);
		Assert.assertEquals(7, tags.size());
		Assert.assertTrue(tags.contains("abc"));
		Assert.assertTrue(tags.contains("ftag2"));
	}

	@Test
	public void testFindDocIdByUserIdAndTag() {
		Collection<Long> ids = dao.findDocIdByUserIdAndTag(1, "abc");
		Assert.assertNotNull(ids);
		// There is also the shortcut
		Assert.assertEquals(1, ids.size());
		Assert.assertEquals(new Long(1), ids.iterator().next());

		ids = dao.findDocIdByUserIdAndTag(1, "xxx");
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());

		ids = dao.findDocIdByUserIdAndTag(99, "abc");
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testFindByUserIdAndTag() {
		List<Document> ids = dao.findByUserIdAndTag(1, "abc", null);
		Assert.assertNotNull(ids);
		// There is also the shortcut
		Assert.assertEquals(1, ids.size());
		Assert.assertEquals(1L, ids.get(0).getId());

		ids = dao.findByUserIdAndTag(4, "zzz", null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(1, ids.size());

		ids = dao.findByUserIdAndTag(1, "xxx", null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());

		ids = dao.findByUserIdAndTag(1, "ask", null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(1, ids.size());

		ids = dao.findByUserIdAndTag(99, "abc", null);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testFindLastDownloadsByUserId() {
		Collection<Document> documents = dao.findLastDownloadsByUserId(1, 5);
		Assert.assertNotNull(documents);
		Assert.assertEquals(2, documents.size());
	}

	@Test
	public void testFindByPath() {
		Document doc = dao.findByPath("/Workspace X/folder6/pluto", 1L);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pluto", doc.getFileName());

		doc = dao.findByPath("/Workspace X/folder6/xyz", 1L);
		Assert.assertNull(doc);
	}

	@Test
	public void testFindByFileNameAndParentFolderId() {
		Collection<Document> documents = dao.findByFileNameAndParentFolderId(6L, "pluto", null, null, null);
		Assert.assertNotNull(documents);
		Assert.assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(6L, "PLUTO", null, 1L, null);
		Assert.assertNotNull(documents);
		Assert.assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(6L, "paperino", null, 1L, null);
		Assert.assertNotNull(documents);
		Assert.assertEquals(0, documents.size());

		Document doc = dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		doc.setFileName("pluto");
		doc.setFolder(folderDao.findById(7));
		dao.store(doc);
		Assert.assertEquals("pluto", doc.getFileName());
		Assert.assertEquals(7, doc.getFolder().getId());

		documents = dao.findByFileNameAndParentFolderId(6L, "pluto", null, 1L, null);
		Assert.assertNotNull(documents);
		Assert.assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(null, "pluto", null, 1L, null);
		Assert.assertNotNull(documents);
		Assert.assertEquals(3, documents.size());
	}

	@Test
	public void testFindLinkedDocuments() {
		Collection<Document> docs = dao.findLinkedDocuments(1, null, null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.size());
		Assert.assertEquals(1, docs.iterator().next().getId());

		docs = dao.findLinkedDocuments(2, "xyz", null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.size());
		Assert.assertEquals(2, docs.iterator().next().getId());

		docs = dao.findLinkedDocuments(2, "xyz", 1);
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.size());
		Assert.assertEquals(2, docs.iterator().next().getId());

		docs = dao.findLinkedDocuments(2, "xyz", 2);
		Assert.assertNotNull(docs);
		Assert.assertEquals(0, docs.size());
	}

	@Test
	public void testFindDeletedDocIds() {
		List<Long> coll = dao.findDeletedDocIds();
		Assert.assertNotNull(coll);
		Assert.assertEquals(3, coll.size());
		Assert.assertTrue(coll.contains(new Long(4)));
	}

	@Test
	public void testFindDeletedDocs() {
		List<Document> coll = dao.findDeletedDocs();
		Assert.assertNotNull(coll);
		Assert.assertEquals(3, coll.size());
	}

	@Test
	public void testCount() {
		Assert.assertEquals(6L, dao.count(null, true, false));
		Assert.assertEquals(3L, dao.count(Tenant.DEFAULT_ID, false, false));
	}

	@Test
	public void testCountByIndexed() {
		Assert.assertEquals(1L, dao.countByIndexed(0));
		Assert.assertEquals(2L, dao.countByIndexed(1));
	}

	@Test
	public void testRestore() {
		Assert.assertNull(dao.findById(4));
		dao.restore(4, 5, null);
		Assert.assertNotNull(dao.findById(4));
		Assert.assertEquals(5L, dao.findById(4).getFolder().getId());
	}

	@Test
	public void testMakeImmutable() {
		History transaction = new History();
		transaction.setFolderId(103);
		transaction.setDocId(2L);
		transaction.setUserId(1);
		transaction.setNotified(0);
		dao.makeImmutable(2, transaction);
		Assert.assertEquals(1, dao.findById(2).getImmutable());
	}

	@Test
	public void testFindByLockUserAndStatus() {
		Assert.assertEquals(3, dao.findByLockUserAndStatus(3L, null).size());
		Assert.assertEquals(2, dao.findByLockUserAndStatus(3L, Document.DOC_CHECKED_OUT).size());
		Assert.assertEquals(2, dao.findByLockUserAndStatus(null, Document.DOC_CHECKED_OUT).size());
		Assert.assertEquals(0, dao.findByLockUserAndStatus(1L, null).size());
		Assert.assertEquals(0, dao.findByLockUserAndStatus(1L, Document.DOC_CHECKED_OUT).size());
		Assert.assertEquals(0, dao.findByLockUserAndStatus(987541L, null).size());
	}

	@Test
	public void testFindAliasIds() {
		Collection<Long> ids = dao.findAliasIds(1);
		Assert.assertNotNull(ids);
		Assert.assertEquals(1, ids.size());
		Assert.assertTrue(ids.contains(new Long(2)));

		ids = dao.findAliasIds(3);
		Assert.assertNotNull(ids);
		Assert.assertEquals(0, ids.size());
	}

	@Test
	public void testSetPassword() throws Exception {
		Document doc = dao.findById(3L);
		Assert.assertNull(doc.getPassword());

		History history = new History();
		history.setUserId(1);
		history.setUsername("admin");
		dao.setPassword(3L, "test", history);

		doc = dao.findById(3L);
		Assert.assertEquals(CryptUtil.cryptString("test"), doc.getPassword());

		dao.unsetPassword(3L, history);
		doc = dao.findById(3L);
		Assert.assertNull(doc.getPassword());
	}

	@Test
	public void testGetWorkspace() {
		Assert.assertNull(dao.getWorkspace(9999));
		Assert.assertEquals(3000L, dao.getWorkspace(3).getId());
	}

	@Test
	public void testFindDeleted() {
		List<Document> deletedDocs = dao.findDeleted(1, 5);
		Assert.assertNotNull(deletedDocs);
		Assert.assertEquals(1, deletedDocs.size());
		Assert.assertEquals("pippo", deletedDocs.get(0).getFileName());

		deletedDocs = dao.findDeleted(2, 4);
		Assert.assertNotNull(deletedDocs);
		Assert.assertEquals(2, deletedDocs.size());

		deletedDocs = dao.findDeleted(1, 1);
		Assert.assertNotNull(deletedDocs);
		Assert.assertEquals(1, deletedDocs.size());
		Assert.assertEquals("pippo", deletedDocs.get(0).getFileName());
	}

	@Test
	public void testFindByIds() {
		List<Document> docs = dao.findByIds(new Long[0], 5);
		Assert.assertNotNull(docs);
		Assert.assertTrue(docs.isEmpty());

		docs = dao.findByIds(new Long[] { 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L }, null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(3, docs.size());
	}

	@Test
	public void testFindDuplicatedDigests() {
		List<String> duplicates = dao.findDuplicatedDigests(1L, 6L);
		Assert.assertEquals(1, duplicates.size());
		Assert.assertEquals("xx", duplicates.iterator().next());
		duplicates = dao.findDuplicatedDigests(1L, 99L);
		Assert.assertEquals(0, duplicates.size());

	}
	
	@Test
	public void testCleanExpiredTransactions() {
		Document doc = dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		doc.setTransactionId("transaction");
		dao.store(doc);

		// The document is now in transaction
		lockManager.get("Test", "transaction");

		dao.cleanExpiredTransactions();
		dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		Assert.assertEquals("transaction", doc.getTransactionId());

		// Now the transaction expired
		lockManager.release("Test", "transaction");

		dao.cleanExpiredTransactions();
		dao.findById(1);
		Assert.assertNotNull(doc);
		dao.initialize(doc);
		Assert.assertNull(doc.getTransactionId());
	}
}