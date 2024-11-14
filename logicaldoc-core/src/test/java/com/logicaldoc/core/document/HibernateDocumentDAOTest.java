package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.lock.LockManager;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateDocumentDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 3.0
 */
public class HibernateDocumentDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentDAO dao;

	private FolderDAO folderDao;

	private LockManager lockManager;

	private TemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		dao = (DocumentDAO) context.getBean("DocumentDAO");

		folderDao = (FolderDAO) context.getBean("FolderDAO");
		lockManager = (LockManager) context.getBean("lockManager");
		templateDao = (TemplateDAO) context.getBean("TemplateDAO");
	}

	@Test
	public void testComputeTotalSize() throws PersistenceException {
		long totalSize = dao.computeTotalSize(1L, null, false);
		assertEquals(22658L, totalSize);

		totalSize = dao.computeTotalSize(1L, null, true);
		assertEquals(22658L, totalSize);

		totalSize = dao.computeTotalSize(1L, 1L, true);
		assertEquals(22658L, totalSize);

		totalSize = dao.computeTotalSize(1L, 3L, true);
		assertEquals(0L, totalSize);

		// Unexisting tenant
		totalSize = dao.computeTotalSize(99L, 1L, true);
		assertEquals(0L, totalSize);
	}

	@Test
	public void testGetTagCloud() throws PersistenceException {
		Session session = SessionManager.get().newSession("admin", "admin", (Client) null);
		try {
			List<TagCloud> cloud = dao.getTagCloud(session.getSid());
			assertNotNull(cloud);
			assertEquals("approved,rejected", cloud.stream().map(TagCloud::getTag).collect(Collectors.joining(",")));
		} finally {
			SessionManager.get().kill(session.getSid());
		}
	}

	@Test
	public void testUpdateDigest() throws PersistenceException {
		Document doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		assertEquals("xx", doc.getDigest());

		File file = new File(rootStoreOne.getAbsolutePath() + "/1/doc/" + doc.getFileVersion());
		assertTrue(file.exists());
		String digest = FileUtil.computeDigest(file);

		dao.updateDigest(doc);
		assertEquals(digest, doc.getDigest());

		Document updatedDoc = dao.findById(1);
		dao.initialize(updatedDoc);
		assertEquals(doc.getVersion(), updatedDoc.getVersion());
	}

	@Test
	public void testDelete() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setEvent(DocumentEvent.DELETED.toString());
		transaction.setComment("");
		transaction.setUser(new User());

		dao.delete(1, transaction);
		Document doc = dao.findById(1);
		assertNull(doc);
	}

	@Test
	public void testArchive() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		dao.archive(1, transaction);
		Document doc = dao.findById(1);
		assertNotNull(doc);
		assertEquals(AbstractDocument.DOC_ARCHIVED, doc.getStatus());
	}

	@Test
	public void testFindArchivedByFolder() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		dao.archive(1, transaction);
		Document doc = dao.findById(1);

		List<Document> docs = dao.findArchivedByFolder(doc.getFolder().getId());
		assertEquals(1, docs.size());
		assertEquals(doc, docs.get(0));
	}

	@Test
	public void testUnArchive() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		dao.archive(1, transaction);
		Document doc = dao.findById(1);
		assertNotNull(doc);
		assertEquals(AbstractDocument.DOC_ARCHIVED, doc.getStatus());

		dao.unarchive(1, transaction);
		doc = dao.findById(1);
		assertNotNull(doc);
		assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<Document> documents = dao.findAll();
		assertNotNull(documents);
		assertEquals(4, documents.size());

		assertEquals(4, dao.findByWhere("1=1", null, null).size());
		assertEquals(1, dao.findByWhere("1=1", null, 1).size());
	}

	@Test
	public void testFindById() throws PersistenceException {
		Document doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		assertEquals(1, doc.getId());
		assertEquals("pippo.pdf", doc.getFileName());
		assertNotNull(doc.getFolder());
		assertEquals(6, doc.getFolder().getId());

		// Try with unexisting document
		doc = dao.findById(99);
		assertNull(doc);
	}

	@Test
	public void testFindByCustomId() throws PersistenceException {
		Document doc = dao.findByCustomId("a", Tenant.DEFAULT_ID);
		assertNotNull(doc);
		dao.initialize(doc);
		assertEquals(1, doc.getId());
		assertEquals("pippo.pdf", doc.getFileName());
		assertNotNull(doc.getFolder());
		assertEquals(6, doc.getFolder().getId());

		// Try with unexisting document
		doc = dao.findByCustomId("xx", Tenant.DEFAULT_ID);
		assertNull(doc);

		// Try with unexisting tenant
		doc = dao.findByCustomId("a", 99L);
		assertNull(doc);
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		Collection<Long> ids = dao.findByUserId(3);
		assertNotNull(ids);
		assertEquals(4, ids.size());
		assertTrue(ids.contains(2L));

		// Try with a user without documents
		ids = dao.findByUserId(2);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindDocIdByFolder() throws PersistenceException {
		Collection<Long> ids = dao.findDocIdByFolder(6, null);
		assertNotNull(ids);
		assertEquals(4, ids.size());
		assertTrue(ids.contains(2L));

		ids = dao.findDocIdByFolder(1111, null);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindByFolder() throws PersistenceException {
		Collection<Document> docs = dao.findByFolder(6, null);
		assertNotNull(docs);
		assertEquals(4, docs.size());
		assertTrue(docs.contains(dao.findById(2)));

		docs = dao.findByFolder(1111, null);
		assertNotNull(docs);
		assertEquals(0, docs.size());
	}

	@Test
	public void testFindIndexed() throws PersistenceException {
		List<Document> docs = dao.findByIndexed(1);
		assertNotNull(docs);
		assertEquals(2, docs.size());
		assertEquals(1, docs.get(0).getId());

		docs = dao.findByIndexed(0);
		assertNotNull(docs);
		assertEquals(1, docs.size());
	}

	@Test
	public void testFindLastModifiedByUserId() throws PersistenceException {
		Collection<Document> coll = dao.findLastModifiedByUserId(1, 10);
		assertNotNull(coll);
		assertEquals(4, coll.size());

		coll = dao.findLastModifiedByUserId(3, 10);
		assertNotNull(coll);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindDocIdByTag() throws PersistenceException {
		Collection<Long> ids = dao.findDocIdByTag("abc");
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertEquals(Long.valueOf(1L), ids.iterator().next());

		ids = dao.findDocIdByTag("xxx");
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testDeleteOrphaned() throws PersistenceException {
		folderDao.delete(6);
		assertNull(folderDao.findById(6));
		dao.deleteOrphaned(1);
		Document doc = dao.findById(1);
		assertTrue(doc == null || doc.getDeleted() == 1);
	}

	@Test
	public void testFindPublishedIds() throws IOException, PersistenceException {
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
		assertTrue(docFile.exists());

		dao.store(doc);

		Set<Long> fids = new HashSet<>();
		fids.add(Folder.ROOTID);

		Collection<Long> ids = dao.findPublishedIds(fids);
		assertTrue(ids.contains(doc.getId()));

		doc.setPublished(0);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		Date pick = cal.getTime();
		doc.setPublished(1);
		doc.setStartPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, -3);
		pick = cal.getTime();
		doc.setStartPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		assertTrue(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		pick = cal.getTime();
		doc.setStopPublishing(pick);
		dao.store(doc);
		ids = dao.findPublishedIds(fids);
		assertFalse(ids.contains(doc.getId()));
	}

	@Test
	public void testMultipleValues() throws PersistenceException {
		Folder folder = folderDao.findById(Folder.ROOTID);
		Template template = templateDao.findById(-1L);

		Document doc = new Document();
		doc.setFolder(folder);
		doc.setTemplate(template);
		doc.setFileName("pippo.pdf");

		doc.setValues("multi", List.of("value1", "value2", "value3"));
		dao.store(doc);

		doc = dao.findById(doc.getId());
		assertNotNull(doc);
		dao.initialize(doc);

		assertEquals("value2", doc.getValue("multi-0001"));
		assertEquals("value3", doc.getValue("multi-0002"));
		assertEquals("multi", doc.getAttribute("multi-0002").getParent());

		assertEquals(3, doc.getValueAttributes("multi").size());

		doc.setValues("multi", List.of("A", "B"));
		dao.store(doc);

		doc = dao.findById(doc.getId());
		assertNotNull(doc);
		dao.initialize(doc);
		assertEquals("B", doc.getValue("multi-0001"));
	}

	@Test
	public void testStore() throws PersistenceException {
		Document doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);

		// Try to store it inside a folder with extended attributes
		Folder folder = folderDao.findById(1202);
		doc.setFolder(folder);

		doc.setValue("object", "test");
		dao.store(doc);
		doc = dao.findById(doc.getId());
		dao.initialize(doc);

		// Check if the defaults were applied
		doc = dao.findById(1);
		dao.initialize(doc);
		assertEquals(1, doc.getTemplate().getId());
		assertEquals("test_val_1", doc.getValue("val1"));

		// Try to store into an alias folder
		Folder alias = folderDao.createAlias(4L, 6L, null);
		doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		doc.setFolder(alias);
		dao.store(doc);

		// The document should be stored in the referenced folder
		doc = dao.findById(1);
		assertNotNull(doc);
		Folder realFolder = folderDao.findById(6L);
		assertEquals(realFolder, doc.getFolder());
	}

	@Test
	public void testFindTags() throws PersistenceException {
		TagsProcessor processor = (TagsProcessor) context.getBean("tagsProcessor");
		processor.run();

		Collection<String> tags = dao.findTags("a", 1L).keySet();
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));
		tags = dao.findTags("a", null).keySet();
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));

		tags = dao.findTags("a", 99L).keySet();
		assertNotNull(tags);
		assertTrue(tags.isEmpty());
	}

	@Test
	public void testFindAllTags() throws PersistenceException {
		TagsProcessor processor = (TagsProcessor) context.getBean("tagsProcessor");
		processor.run();

		Collection<String> tags = dao.findAllTags("a", 1L);
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));
		tags = dao.findAllTags(null, 1L);
		assertNotNull(tags);
		assertEquals(7, tags.size());
		assertTrue(tags.contains("abc"));
		assertTrue(tags.contains("ftag2"));
	}

	@Test
	public void testFindDocIdByUserIdAndTag() throws PersistenceException {
		Collection<Long> ids = dao.findDocIdByUserIdAndTag(1, "abc");
		assertNotNull(ids);
		// There is also the shortcut
		assertEquals(1, ids.size());
		assertEquals(Long.valueOf(1L), ids.iterator().next());

		ids = dao.findDocIdByUserIdAndTag(1, "xxx");
		assertNotNull(ids);
		assertEquals(0, ids.size());

		ids = dao.findDocIdByUserIdAndTag(99, "abc");
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindByUserIdAndTag() throws PersistenceException {
		List<Document> ids = dao.findByUserIdAndTag(1, "abc", null);
		assertNotNull(ids);
		// There is also the shortcut
		assertEquals(1, ids.size());
		assertEquals(1L, ids.get(0).getId());

		ids = dao.findByUserIdAndTag(4, "zzz", null);
		assertNotNull(ids);
		assertEquals(1, ids.size());

		ids = dao.findByUserIdAndTag(1, "xxx", null);
		assertNotNull(ids);
		assertEquals(0, ids.size());

		ids = dao.findByUserIdAndTag(1, "ask", null);
		assertNotNull(ids);
		assertTrue(ids.isEmpty());

		ids = dao.findByUserIdAndTag(99, "abc", null);
		assertNotNull(ids);
		assertTrue(ids.isEmpty());
	}

	@Test
	public void testFindLastDownloadsByUserId() throws PersistenceException {
		Collection<Document> documents = dao.findLastDownloadsByUserId(1, 5);
		assertNotNull(documents);
		assertEquals(2, documents.size());
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		Document doc = dao.findByPath("/Workspace X/folder6/pluto", 1L);
		assertNotNull(doc);
		assertEquals("pluto", doc.getFileName());

		doc = dao.findByPath("/Workspace X/folder6/xyz", 1L);
		assertNull(doc);
	}

	@Test
	public void testFindByFileNameAndParentFolderId() throws PersistenceException {
		Collection<Document> documents = dao.findByFileNameAndParentFolderId(6L, "pluto", null, null, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(6L, "PLUTO", null, 1L, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(6L, "paperino", null, 1L, null);
		assertNotNull(documents);
		assertEquals(0, documents.size());

		Document doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		doc.setFileName("pluto");
		doc.setFolder(folderDao.findById(7));
		dao.store(doc);
		assertEquals("pluto", doc.getFileName());
		assertEquals(7, doc.getFolder().getId());

		documents = dao.findByFileNameAndParentFolderId(6L, "pluto", null, 1L, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = dao.findByFileNameAndParentFolderId(null, "pluto", null, 1L, null);
		assertNotNull(documents);
		assertEquals(3, documents.size());
	}

	@Test
	public void testFindLinkedDocuments() throws PersistenceException {
		Collection<Document> docs = dao.findLinkedDocuments(1, null, null);
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals(3, docs.iterator().next().getId());

		docs = dao.findLinkedDocuments(3, "xyz", 1);
		assertNotNull(docs);
		assertEquals(1, docs.size());

		docs = dao.findLinkedDocuments(3, "xyz", 2);
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals(1, docs.iterator().next().getId());
	}

	@Test
	public void testFindDeletedDocIds() throws PersistenceException {
		List<Long> coll = dao.findDeletedDocIds();
		assertNotNull(coll);
		assertEquals(3, coll.size());
		assertTrue(coll.contains(Long.valueOf(4L)));
	}

	@Test
	public void testFindDeletedDocs() throws PersistenceException {
		List<Document> coll = dao.findDeletedDocs();
		assertNotNull(coll);
		assertEquals(3, coll.size());
	}

	@Test
	public void testCount() throws PersistenceException {
		assertEquals(7L, dao.count(null, true, false));
		assertEquals(4L, dao.count(Tenant.DEFAULT_ID, false, false));
	}

	@Test
	public void testCountByIndexed() throws PersistenceException {
		assertEquals(2L, dao.countByIndexed(0));
		assertEquals(2L, dao.countByIndexed(1));
	}

	@Test
	public void testRestore() throws PersistenceException {
		assertNull(dao.findById(4));
		dao.restore(4, 5, null);
		assertNotNull(dao.findById(4));
		assertEquals(5L, dao.findById(4).getFolder().getId());
	}

	@Test
	public void testMakeImmutable() throws PersistenceException {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setDocId(2L);
		transaction.setUserId(1L);
		transaction.setNotified(0);
		dao.makeImmutable(2, transaction);
		assertEquals(1, dao.findById(2).getImmutable());
	}

	@Test
	public void testFindByLockUserAndStatus() {
		assertEquals(3, dao.findByLockUserAndStatus(3L, null).size());
		assertEquals(2, dao.findByLockUserAndStatus(3L, AbstractDocument.DOC_CHECKED_OUT).size());
		assertEquals(3, dao.findByLockUserAndStatus(null, AbstractDocument.DOC_CHECKED_OUT).size());
		assertEquals(1, dao.findByLockUserAndStatus(1L, null).size());
		assertEquals(1, dao.findByLockUserAndStatus(1L, AbstractDocument.DOC_CHECKED_OUT).size());
		assertEquals(0, dao.findByLockUserAndStatus(987541L, null).size());
	}

	@Test
	public void testFindAliasIds() throws PersistenceException {
		Collection<Long> ids = dao.findAliasIds(1);
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertTrue(ids.contains(Long.valueOf(2L)));

		ids = dao.findAliasIds(3);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testSetPassword() throws Exception {
		Document doc = dao.findById(3L);
		assertNull(doc.getPassword());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		dao.setPassword(3L, "test", history);

		doc = dao.findById(3L);
		assertEquals(CryptUtil.encryptSHA256("test"), doc.getPassword());

		dao.unsetPassword(3L, history);
		doc = dao.findById(3L);
		assertNull(doc.getPassword());
	}

	@Test
	public void testGetWorkspace() throws PersistenceException {
		assertNull(dao.getWorkspace(9999));
		assertEquals(3000L, dao.getWorkspace(3).getId());
	}

	@Test
	public void testFindDeleted() throws PersistenceException {
		List<Document> deletedDocs = dao.findDeleted(1, 5);
		assertNotNull(deletedDocs);
		assertEquals(1, deletedDocs.size());
		assertEquals("pippo", deletedDocs.get(0).getFileName());

		deletedDocs = dao.findDeleted(2, 4);
		assertNotNull(deletedDocs);
		assertEquals(2, deletedDocs.size());

		deletedDocs = dao.findDeleted(1, 1);
		assertNotNull(deletedDocs);
		assertEquals(1, deletedDocs.size());
		assertEquals("pippo", deletedDocs.get(0).getFileName());
	}

	@Test
	public void testFindByIds() {
		List<Document> docs = dao.findByIds(new HashSet<>(), 5);
		assertNotNull(docs);
		assertTrue(docs.isEmpty());

		docs = dao.findByIds(Set.of(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), null);
		assertNotNull(docs);
		assertEquals(4, docs.size());
	}

	@Test
	public void testFindDuplicatedDigests() throws PersistenceException {
		List<String> duplicates = dao.findDuplicatedDigests(1L, 6L);
		assertEquals(1, duplicates.size());
		assertEquals("xx", duplicates.iterator().next());
		duplicates = dao.findDuplicatedDigests(1L, 99L);
		assertEquals(0, duplicates.size());

	}

	@Test
	public void testCleanExpiredTransactions() throws PersistenceException {
		Document doc = dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		doc.setTransactionId("transaction");
		dao.store(doc);

		// The document is now in transaction
		lockManager.get("Test", "transaction");

		dao.cleanExpiredTransactions();
		dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		assertEquals("transaction", doc.getTransactionId());

		// Now the transaction expired
		lockManager.release("Test", "transaction");

		dao.cleanExpiredTransactions();
		dao.findById(1);
		assertNotNull(doc);
		dao.initialize(doc);
		assertNull(doc.getTransactionId());
	}

	@Test
	public void testCleanUnexistingUniqueTags() throws PersistenceException {
		assertEquals(2, dao.queryForInt("select count(*) from ld_uniquetag"));
		dao.cleanUnexistingUniqueTags();
		assertEquals(0, dao.queryForInt("select count(*) from ld_uniquetag"));
	}

	@Test
	public void testCleanUnexistingUniqueTagsOneByOne() throws PersistenceException {
		assertEquals(2, dao.queryForInt("select count(*) from ld_uniquetag"));
		dao.cleanUnexistingUniqueTagsOneByOne();
		assertEquals(0, dao.queryForInt("select count(*) from ld_uniquetag"));
	}

	@Test
	public void testGetAllowedPermissions() throws PersistenceException {
		assertTrue(dao.isReadAllowed(7L, 3L));
		assertTrue(dao.isWriteAllowed(7L, 3L));
		assertTrue(dao.isPrintAllowed(7L, 3L));

		assertTrue(dao.getAllowedPermissions(7L, 3L).contains(Permission.SECURITY));
		assertTrue(!dao.getAllowedPermissions(7L, 2L).contains(Permission.SECURITY));
	}

	@Test
	public void testApplyParentFolderSecurity() throws PersistenceException {
		Document doc = dao.findById(1L);
		dao.initialize(doc);
		assertTrue(doc.getAccessControlList().isEmpty());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUserId(1L);
		transaction.setUsername("admin");
		dao.applyParentFolderSecurity(1L, transaction);

		doc = dao.findById(1L);
		dao.initialize(doc);
		folderDao.initialize(doc.getFolder());
		assertTrue(!doc.getAccessControlList().isEmpty());
		assertEquals(doc.getFolder().getAccessControlList().size(), doc.getAccessControlList().size());
	}
}