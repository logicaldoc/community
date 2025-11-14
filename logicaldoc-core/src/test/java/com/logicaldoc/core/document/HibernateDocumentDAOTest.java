package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
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
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateDocumentDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 3.0
 */
public class HibernateDocumentDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentDAO testSubject;

	private FolderDAO folderDao;

	private LockManager lockManager;

	private TemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		testSubject = DocumentDAO.get();
		folderDao = FolderDAO.get();
		lockManager = Context.get(LockManager.class);
		templateDao = TemplateDAO.get();
	}

	@Test
	public void testComputeTotalSize() throws PersistenceException {
		long totalSize = testSubject.computeTotalSize(1L, null, false);
		assertEquals(22658L, totalSize);

		totalSize = testSubject.computeTotalSize(1L, null, true);
		assertEquals(22658L, totalSize);

		totalSize = testSubject.computeTotalSize(1L, 1L, true);
		assertEquals(22658L, totalSize);

		totalSize = testSubject.computeTotalSize(1L, 3L, true);
		assertEquals(0L, totalSize);

		// Non-existent tenant
		totalSize = testSubject.computeTotalSize(99L, 1L, true);
		assertEquals(0L, totalSize);
	}

	@Test
	public void testGetTagCloud() throws PersistenceException {
		Session session = SessionManager.get().newSession("admin", "admin", (Client) null);

		try {
			testSubject.updateCountUniqueTags();

			List<TagCloud> cloud = testSubject.getTagCloud(session.getSid());
			assertNotNull(cloud);
			assertEquals("approved,rejected", cloud.stream().map(TagCloud::getTag).collect(Collectors.joining(",")));

		} finally {
			SessionManager.get().kill(session.getSid());
		}

		// test TagCloud class methods
		TagCloud tag1 = new TagCloud();
		tag1.setTag("abc");
		assertNotNull(tag1);

		TagCloud tag2 = new TagCloud("bcd");
		tag2.setTag("cde");
		tag2.setCount(2);
		assertNotNull(tag2);
		assertEquals(0, tag2.getScale());

		assertNotSame(tag1.hashCode(), tag2.hashCode());

		assertEquals(true, tag1.equals(tag1));
		assertEquals(false, tag1.equals(tag2));
		assertEquals(false, tag1.equals(null));
		assertEquals(false, tag1.equals(new Object()));

		tag2.setTag(null);
		TagCloud tag3 = new TagCloud("bcd");
		assertEquals(false, tag2.equals(tag3));
	}

	@Test
	public void testUpdateDigest() throws PersistenceException {
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertEquals("xx", doc.getDigest());

		File file = new File(rootStoreOne.getAbsolutePath() + "/1/doc/" + doc.getFileVersion());
		assertTrue(file.exists());
		String digest = FileUtil.computeDigest(file);

		testSubject.updateDigest(doc);
		assertEquals(digest, doc.getDigest());

		Document updatedDoc = testSubject.findById(1);
		testSubject.initialize(updatedDoc);
		assertEquals(doc.getVersion(), updatedDoc.getVersion());

		doc = testSubject.findById(0);
		testSubject.initialize(doc);
		assertEquals(null, doc);
	}

	@Test
	public void testDelete() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setEvent(DocumentEvent.DELETED);
		transaction.setComment("");
		transaction.setUser(new User());

		testSubject.delete(1, transaction);
		Document doc = testSubject.findById(1);
		assertNull(doc);
	}

	@Test
	public void testDeleteAll() throws PersistenceException {
		Folder testFolder = folderDao.findById(5);
		folderDao.initialize(testFolder);
		assertNotSame(null, testFolder);

		Document doc1 = new Document();
		doc1.setFileName("TestDocument1");
		doc1.setFolder(testFolder);
		testSubject.store(doc1);
		assertNotSame(null, doc1);

		Document doc2 = new Document();
		doc2.setFileName("TestDocument2");
		doc2.setFolder(testFolder);
		testSubject.store(doc2);
		assertNotSame(null, doc2);

		Document retrievedDoc1 = testSubject.findById(doc1.getId());
		Document retrievedDoc2 = testSubject.findById(doc2.getId());

		assertNotNull(retrievedDoc1);
		assertNotNull(retrievedDoc2);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setEvent(DocumentEvent.DELETED);
		transaction.setUser(new User());

		List<Document> documentsToDelete = Arrays.asList(retrievedDoc1, retrievedDoc2);
		testSubject.deleteAll(documentsToDelete, transaction);

		assertNull(testSubject.findById(doc1.getId()));
		assertNull(testSubject.findById(doc2.getId()));
	}

	@Test
	public void testArchive() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		testSubject.archive(1, transaction);
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		assertEquals(DocumentStatus.ARCHIVED, doc.getStatus());
	}

	@Test
	public void testFindArchivedByFolder() throws PersistenceException {
		// Create the document history event
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		testSubject.archive(1, transaction);
		Document doc = testSubject.findById(1);

		List<Document> docs = testSubject.findArchivedByFolder(doc.getFolder().getId());
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

		testSubject.archive(1, transaction);
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		assertEquals(DocumentStatus.ARCHIVED, doc.getStatus());

		testSubject.unarchive(1, transaction);
		doc = testSubject.findById(1);
		assertNotNull(doc);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<Document> documents = testSubject.findAll();
		assertNotNull(documents);
		assertEquals(6, documents.size());

		assertEquals(6, testSubject.findByWhere("1=1", null, null).size());
		assertEquals(1, testSubject.findByWhere("1=1", null, 1).size());
	}

	@Test
	public void testFindById() throws PersistenceException {
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertEquals(1, doc.getId());
		assertEquals("pippo.pdf", doc.getFileName());
		assertNotNull(doc.getFolder());
		assertEquals(6, doc.getFolder().getId());

		assertEquals(true, testSubject.isDownloadAllowed(1, 1));

		// Try with non-existing document
		doc = testSubject.findById(99);
		assertNull(doc);
	}

	@Test
	public void testFindByCustomId() throws PersistenceException {
		Document doc = testSubject.findByCustomId("a", Tenant.DEFAULT_ID);
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertEquals(1, doc.getId());
		assertEquals("pippo.pdf", doc.getFileName());
		assertNotNull(doc.getFolder());
		assertEquals(6, doc.getFolder().getId());
		assertEquals(true, testSubject.isMoveAllowed(doc.getId(), 1L));
		assertEquals(true, testSubject.isPreviewAllowed(doc.getId(), 1L));

		// Try with non-existing document
		doc = testSubject.findByCustomId("xx", Tenant.DEFAULT_ID);
		assertNull(doc);

		// Try with non-existing tenant
		doc = testSubject.findByCustomId("a", 99L);
		assertNull(doc);
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		Collection<Long> ids = testSubject.findByUserId(3);
		assertNotNull(ids);
		assertEquals(6, ids.size());
		assertTrue(ids.contains(2L));

		// Try with a user without documents
		ids = testSubject.findByUserId(2L);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindDocIdByFolder() throws PersistenceException {
		Collection<Long> ids = testSubject.findDocIdByFolder(6, null);
		assertNotNull(ids);
		assertEquals(6, ids.size());
		assertTrue(ids.contains(2L));

		ids = testSubject.findDocIdByFolder(1111, null);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindByFolder() throws PersistenceException {
		Collection<Document> docs = testSubject.findByFolder(6, null);
		assertNotNull(docs);
		assertEquals(6, docs.size());
		assertTrue(docs.contains(testSubject.findById(2)));

		docs = testSubject.findByFolder(1111, null);
		assertNotNull(docs);
		assertEquals(0, docs.size());
	}

	@Test
	public void testFindDocument() throws PersistenceException {
		Folder testFolder = folderDao.findById(6);
		folderDao.initialize(testFolder);
		assertNotSame(null, testFolder);

		Document doc1 = new Document();
		doc1.setFileName("test.pdf");
		doc1.setFolder(testFolder);
		doc1.setDocRef(1L);
		testSubject.store(doc1);
		assertNotSame(null, doc1);

		assertNotNull(testSubject.findDocument(doc1.getId()));
	}

	@Test
	public void testFindByIndexingStatus() throws PersistenceException {
		List<Document> docs = testSubject.findByIndexingStatus(IndexingStatus.INDEXED);
		assertNotNull(docs);
		assertEquals(4, docs.size());
		assertEquals(1, docs.get(0).getId());

		docs = testSubject.findByIndexingStatus(IndexingStatus.TO_INDEX);
		assertNotNull(docs);
		assertEquals(1, docs.size());
	}

	@Test
	public void testFindLastModifiedByUserId() throws PersistenceException {
		Collection<Document> coll = testSubject.findLastModifiedByUserId(1, 10);
		assertNotNull(coll);
		assertEquals(4, coll.size());

		coll = testSubject.findLastModifiedByUserId(3, 10);
		assertNotNull(coll);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindDocIdByTag() throws PersistenceException {
		Collection<Long> ids = testSubject.findDocIdByTag("abc");
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertEquals(Long.valueOf(1L), ids.iterator().next());

		ids = testSubject.findDocIdByTag("xxx");
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testDeleteOrphaned() throws PersistenceException {
		folderDao.delete(6);
		assertNull(folderDao.findById(6));
		testSubject.deleteOrphaned(1);
		Document doc = testSubject.findById(1);
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

		testSubject.store(doc);

		Set<Long> fids = new HashSet<>();
		fids.add(Folder.ROOTID);

		Collection<Long> ids = testSubject.findPublishedIds(fids);
		assertTrue(ids.contains(doc.getId()));

		doc.setPublished(false);
		testSubject.store(doc);
		ids = testSubject.findPublishedIds(fids);
		assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		Date pick = cal.getTime();
		doc.setPublished(true);
		doc.setStartPublishing(pick);
		testSubject.store(doc);
		ids = testSubject.findPublishedIds(fids);
		assertFalse(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, -3);
		pick = cal.getTime();
		doc.setStartPublishing(pick);
		testSubject.store(doc);
		ids = testSubject.findPublishedIds(fids);
		assertTrue(ids.contains(doc.getId()));

		cal.add(Calendar.DATE, 1);
		pick = cal.getTime();
		doc.setStopPublishing(pick);
		testSubject.store(doc);
		ids = testSubject.findPublishedIds(fids);
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
		testSubject.store(doc);

		doc = testSubject.findById(doc.getId());
		assertNotNull(doc);
		testSubject.initialize(doc);

		assertEquals("value2", doc.getValue("multi-0001"));
		assertEquals("value3", doc.getValue("multi-0002"));
		assertEquals("multi", doc.getAttribute("multi-0002").getParent());

		assertEquals(3, doc.getValueAttributes("multi").size());

		doc.setValues("multi", List.of("A", "B"));
		testSubject.store(doc);

		doc = testSubject.findById(doc.getId());
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertEquals("B", doc.getValue("multi-0001"));
	}

	@Test
	public void testStoreMassive() throws PersistenceException {
		assertEquals(0L, testSubject.queryForLong("select count(*) from ld_document where ld_filename like 'test-%'"));

		int total = Context.get().getProperties().getInt("maxdocsperfolder") - 100;
		Document master = testSubject.findById(1);
		testSubject.initialize(master);
		for (int i = 0; i < total; i++) {
			Document newDoc = new Document();
			newDoc.setFileName("test-" + i + ".txt");
			newDoc.setId(0L);
			newDoc.setCustomId(null);
			newDoc.setFolder(master.getFolder());
			testSubject.store(newDoc);
		}

		assertEquals(total,
				testSubject.queryForLong("select count(*) from ld_document where ld_filename like 'test-%'"));
	}

	@Test
	public void testStore() throws PersistenceException {
		Document doc = testSubject.findById(1L);
		assertNotNull(doc);
		testSubject.initialize(doc);

		// Try to store it inside a folder with extended attributes
		Folder folder = folderDao.findById(1202L);
		doc.setFolder(folder);

		doc.setValue("object", "test");
		testSubject.store(doc);

		// Check if the defaults were applied
		assertEquals(1, doc.getTemplate().getId());
		assertEquals("test_val_1", doc.getValue("val1"));

		// Try to store into an alias folder
		Folder alias = folderDao.createAlias(4L, 6L, null);
		doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		doc.setFolder(alias);
		testSubject.store(doc);

		assertNotNull(testSubject.findById(doc.getId()).getLastModified());

		// The document should be stored in the referenced folder
		doc = testSubject.findById(1);
		testSubject.initialize(doc);
		assertNotNull(doc);
		Folder realFolder = folderDao.findById(6L);
		assertEquals(realFolder, doc.getFolder());

		// Ocr template at folder level should be present
		doc.setFileName("test123");
		doc.getFolder().setBarcodeTemplateId(1L);
		doc.getFolder().setOcrTemplateId(1L);
		testSubject.store(doc);

		// The document template should be null
		doc.setFileName("test123");
		doc.getFolder().setBarcodeTemplateId(null);
		doc.getFolder().setOcrTemplateId(null);
		doc.setTemplate(null);
		testSubject.store(doc);

		// The document has id == 0 and number of documents in the folder > 0
		Context.get().getProperties().setProperty("maxdocsperfolder", "3");
		Folder folder2 = folderDao.findById(6);
		doc = new Document();
		doc.setFileName("newDoc");
		doc.setFolder(folder2);

		try {
			testSubject.store(doc);
			fail("Expected TooManyDocumentsException but none was thrown");
		} catch (TooManyDocumentsException e) {
			// exception expected
		} catch (Exception e) {
			fail("Unexpected exception: " + e.getMessage());
		}
	}

	@Test
	public void testFindTags() throws PersistenceException {
		TagsProcessor processor = (TagsProcessor) context.getBean("tagsProcessor");
		processor.run();

		Collection<String> tags = testSubject.findTags("a", 1L).keySet();
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));
		tags = testSubject.findTags("a", null).keySet();
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));

		tags = testSubject.findTags("a", 99L).keySet();
		assertNotNull(tags);
		assertTrue(tags.isEmpty());

		tags = testSubject.findTags(1L);
		assertNotNull(tags);

		// testing Tag class methods
		Document doc1 = testSubject.findById(1);
		testSubject.initialize(doc1);
		assertNotNull(doc1);

		Tag tag1 = doc1.getTags().stream().findFirst().get();
		Tag tag2 = doc1.getTags().stream().skip(1).findFirst().orElse(null);

		assertTrue(tag1.compareTo(tag2) < 0);
		assertEquals(true, tag1.compareTo(tag1) == 0);
		assertNotNull(tag1.toString());

		String nullTag = null;
		tag1.setTag(nullTag);
		assertNotNull(tag1.toString());
		assertNotSame(tag1.hashCode(), tag2.hashCode());

		assertEquals(false, tag1.equals(null));
		assertEquals(false, tag2.equals(new Object()));

		Tag tag3 = new Tag(1L, "tyi");
		assertNotNull(tag3);
	}

	@Test
	public void tetstFindAllTags() throws PersistenceException {
		TagsProcessor processor = (TagsProcessor) context.getBean("tagsProcessor");
		processor.run();

		assertEquals(false, processor.isIndeterminate());

		Collection<String> tags = testSubject.findAllTags("a", 1L);
		assertNotNull(tags);
		assertEquals(1, tags.size());
		assertTrue(tags.contains("abc"));
		tags = testSubject.findAllTags(null, 1L);
		assertNotNull(tags);
		assertEquals(7, tags.size());
		assertTrue(tags.contains("abc"));
		assertTrue(tags.contains("ftag2"));
	}

	@Test
	public void testFindDocIdByUserIdAndTag() throws PersistenceException {
		Collection<Long> ids = testSubject.findDocIdByUserIdAndTag(1, "abc");
		assertNotNull(ids);
		// There is also the shortcut
		assertEquals(1, ids.size());
		assertEquals(Long.valueOf(1L), ids.iterator().next());

		ids = testSubject.findDocIdByUserIdAndTag(1, "xxx");
		assertNotNull(ids);
		assertEquals(0, ids.size());

		ids = testSubject.findDocIdByUserIdAndTag(99, "abc");
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testFindByUserIdAndTag() throws PersistenceException {
		List<Document> ids = testSubject.findByUserIdAndTag(1, "abc", null);
		assertNotNull(ids);
		// There is also the shortcut
		assertEquals(1, ids.size());
		assertEquals(1L, ids.get(0).getId());

		ids = testSubject.findByUserIdAndTag(4, "zzz", null);
		assertNotNull(ids);
		assertEquals(1, ids.size());

		ids = testSubject.findByUserIdAndTag(1, "xxx", null);
		assertNotNull(ids);
		assertEquals(0, ids.size());

		ids = testSubject.findByUserIdAndTag(1, "ask", null);
		assertNotNull(ids);
		assertEquals(1, ids.size());

		ids = testSubject.findByUserIdAndTag(99, "abc", null);
		assertNotNull(ids);
		assertTrue(ids.isEmpty());
	}

	@Test
	public void testFindLastDownloadsByUserId() throws PersistenceException {
		Collection<Document> documents = testSubject.findLastDownloadsByUserId(1, 5);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		// testing if document is empty
		documents = testSubject.findLastDownloadsByUserId(0, 0);
		assertEquals(0, documents.size());

		// testing if document is empty
		documents = testSubject.findLastDownloadsByUserId(1, 1);
		assertEquals(2, documents.size());
	}

	@Test
	public void testFindByPath() throws PersistenceException {
		Document doc = testSubject.findByPath("/Workspace X/folder6/pluto", 1L);
		assertNotNull(doc);
		assertEquals("pluto", doc.getFileName());

		doc = testSubject.findByPath("/Workspace X/folder6/xyz", 1L);
		assertNull(doc);

		String invalidPath = "invalidpath";

		Document retrievedDoc = testSubject.findByPath(invalidPath, 1L);
		assertNull(retrievedDoc);
	}

	@Test
	public void testFindByFileNameAndParentFolderId() throws PersistenceException {
		Collection<Document> documents = testSubject.findByFileNameAndParentFolderId(6L, "pluto", null, null, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = testSubject.findByFileNameAndParentFolderId(6L, "PLUTO", null, 1L, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = testSubject.findByFileNameAndParentFolderId(6L, "paperino", null, 1L, null);
		assertNotNull(documents);
		assertEquals(0, documents.size());

		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		doc.setFileName("pluto");
		doc.setFolder(folderDao.findById(7));
		testSubject.store(doc);
		assertEquals("pluto", doc.getFileName());
		assertEquals(7, doc.getFolder().getId());

		documents = testSubject.findByFileNameAndParentFolderId(6L, "pluto", null, 1L, null);
		assertNotNull(documents);
		assertEquals(2, documents.size());

		documents = testSubject.findByFileNameAndParentFolderId(null, "pluto", null, 1L, null);
		assertNotNull(documents);
		assertEquals(3, documents.size());

		// with excludedId != null
		documents = testSubject.findByFileNameAndParentFolderId(6L, "pluto", 1L, 1L, null);
		assertNotNull(documents);
		;
	}

	@Test
	public void testFindLinkedDocuments() throws PersistenceException {
		Collection<Document> docs = testSubject.findLinkedDocuments(1, null, null);
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals(3, docs.iterator().next().getId());

		docs = testSubject.findLinkedDocuments(3, "xyz", 1);
		assertNotNull(docs);
		assertEquals(1, docs.size());

		docs = testSubject.findLinkedDocuments(3, "xyz", 2);
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals(1, docs.iterator().next().getId());

		docs = testSubject.findLinkedDocuments(0, "xyz", 2);
		assertNotNull(docs);
	}

	@Test
	public void testFindDeletedDocIds() throws PersistenceException {
		List<Long> coll = testSubject.findDeletedDocIds();
		assertNotNull(coll);
		assertEquals(3, coll.size());
		assertTrue(coll.contains(Long.valueOf(4L)));
	}

	@Test
	public void testFindDeletedDocs() throws PersistenceException {
		List<Document> coll = testSubject.findDeletedDocs();
		assertNotNull(coll);
		assertEquals(3, coll.size());
	}

	@Test
	public void testCount() throws PersistenceException {
		assertEquals(9L, testSubject.count(null, true, false));
		assertEquals(6L, testSubject.count(Tenant.DEFAULT_ID, false, false));
	}

	@Test
	public void testCountByIndexed() throws PersistenceException {
		assertEquals(2L, testSubject.countByIndexed(IndexingStatus.TO_INDEX));
		assertEquals(4L, testSubject.countByIndexed(IndexingStatus.INDEXED));
	}

	@Test
	public void testRestore() throws PersistenceException {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setSessionId("123");
		transaction.setComment("");
		transaction.setUser(new User());

		assertNull(testSubject.findById(4));
		testSubject.restore(4, 5, transaction);
		assertNotNull(testSubject.findById(4));
		assertEquals(5L, testSubject.findById(4).getFolder().getId());

	}

	@Test
	public void testMakeImmutable() throws PersistenceException {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(103L);
		transaction.setDocId(2L);
		transaction.setUserId(1L);
		transaction.setNotified(false);
		testSubject.makeImmutable(2, transaction);
		assertTrue(testSubject.findById(2).isImmutable());
	}

	@Test
	public void testFindByLockUserAndStatus() {
		assertEquals(5, testSubject.findByLockUserAndStatus(3L, null).size());
		assertEquals(4, testSubject.findByLockUserAndStatus(3L, DocumentStatus.CHECKEDOUT).size());
		assertEquals(5, testSubject.findByLockUserAndStatus(null, DocumentStatus.CHECKEDOUT).size());
		assertEquals(1, testSubject.findByLockUserAndStatus(1L, null).size());
		assertEquals(1, testSubject.findByLockUserAndStatus(1L, DocumentStatus.CHECKEDOUT).size());
		assertEquals(0, testSubject.findByLockUserAndStatus(987541L, null).size());
	}

	@Test
	public void testFindAliasIds() throws PersistenceException {
		Collection<Long> ids = testSubject.findAliasIds(1);
		assertNotNull(ids);
		assertEquals(1, ids.size());
		assertTrue(ids.contains(Long.valueOf(2L)));

		ids = testSubject.findAliasIds(3);
		assertNotNull(ids);
		assertEquals(0, ids.size());
	}

	@Test
	public void testSetPassword() throws Exception {
		Document doc = testSubject.findById(3L);
		assertNull(doc.getPassword());

		DocumentHistory history = new DocumentHistory();
		history.setUserId(1L);
		history.setUsername("admin");
		testSubject.setPassword(3L, "test", history);

		doc = testSubject.findById(3L);
		assertEquals(CryptUtil.encryptSHA256("test"), doc.getPassword());

		testSubject.unsetPassword(3L, history);
		doc = testSubject.findById(3L);
		assertNull(doc.getPassword());
	}

	@Test
	public void testGetWorkspace() throws PersistenceException {
		assertNull(testSubject.getWorkspace(9999));
		assertEquals(3000L, testSubject.getWorkspace(3).getId());
	}

	@Test
	public void testFindDeleted() throws PersistenceException {
		List<Document> deletedDocs = testSubject.findDeleted(1, 5);
		assertNotNull(deletedDocs);
		assertEquals(1, deletedDocs.size());
		assertEquals("pippo", deletedDocs.get(0).getFileName());

		deletedDocs = testSubject.findDeleted(2, 4);
		assertNotNull(deletedDocs);
		assertEquals(2, deletedDocs.size());

		deletedDocs = testSubject.findDeleted(1, 1);
		assertNotNull(deletedDocs);
		assertEquals(1, deletedDocs.size());
		assertEquals("pippo", deletedDocs.get(0).getFileName());
	}

	@Test
	public void testFindByIds() {
		List<Document> docs = testSubject.findByIds(new HashSet<>(), 5);
		assertNotNull(docs);
		assertTrue(docs.isEmpty());

		docs = testSubject.findByIds(Set.of(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), null);
		assertNotNull(docs);
		assertEquals(5, docs.size());
	}

	@Test
	public void testFindDuplicatedDigests() throws PersistenceException {
		List<String> duplicates = testSubject.findDuplicatedDigests(1L, 6L);
		assertEquals(1, duplicates.size());
		assertEquals("xx", duplicates.iterator().next());
		duplicates = testSubject.findDuplicatedDigests(1L, 99L);
		assertEquals(0, duplicates.size());

	}

	@Test
	public void testCleanExpiredTransactions() throws PersistenceException {
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		doc.setTransactionId("transaction");
		testSubject.store(doc);

		// The document is now in transaction
		lockManager.get("Test", "transaction");

		testSubject.cleanExpiredTransactions();
		testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertEquals("transaction", doc.getTransactionId());

		// Now the transaction expired
		lockManager.release("Test", "transaction");

		testSubject.cleanExpiredTransactions();
		testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);
		assertNull(doc.getTransactionId());
	}

	@Test
	public void testCleanUnexistingUniqueTags() throws PersistenceException {
		assertEquals(2, testSubject.queryForInt("select count(*) from ld_uniquetag"));
		testSubject.cleanUnexistingUniqueTags();
		assertEquals(0, testSubject.queryForInt("select count(*) from ld_uniquetag"));
	}

	@Test
	public void testCleanUnexistingUniqueTagsOneByOne() throws PersistenceException {
		assertEquals(2, testSubject.queryForInt("select count(*) from ld_uniquetag"));
		testSubject.cleanUnexistingUniqueTagsOneByOne();
		assertEquals(0, testSubject.queryForInt("select count(*) from ld_uniquetag"));
	}

	@Test
	public void testGetAllowedPermissions() throws PersistenceException {
		assertTrue(testSubject.isReadAllowed(7L, 3L));
		assertTrue(testSubject.isWriteAllowed(7L, 3L));
		assertTrue(testSubject.isPrintAllowed(7L, 3L));

		assertFalse(testSubject.getAllowedPermissions(7L, 2L).contains(Permission.IMMUTABLE));
		assertTrue(testSubject.getAllowedPermissions(7L, 2L).contains(Permission.SECURITY));

		// Testing document without permissions
		assertEquals(Collections.emptySet(), testSubject.getAllowedPermissions(8L, 2L));
	}

	@Test
	public void testApplyParentFolderSecurity() throws PersistenceException {
		Document doc = testSubject.findById(1L);
		testSubject.initialize(doc);
		assertTrue(doc.getAccessControlList().isEmpty());

		DocumentHistory transaction = new DocumentHistory();
		transaction.setUserId(1L);
		transaction.setUsername("admin");
		testSubject.applyParentFolderSecurity(1L, transaction);

		doc = testSubject.findById(1L);
		testSubject.initialize(doc);
		folderDao.initialize(doc.getFolder());
		assertTrue(!doc.getAccessControlList().isEmpty());
		assertEquals(doc.getFolder().getAccessControlList().size(), doc.getAccessControlList().size());

		// folder securityRef != null
		doc = testSubject.findById(1L);
		testSubject.initialize(doc);
		Folder folder = doc.getFolder();
		folderDao.initialize(doc.getFolder());
		folder.setSecurityRef(1202L);
		doc.getFolder().setSecurityRef(1202L);
		folderDao.store(doc.getFolder());
		testSubject.applyParentFolderSecurity(1L, transaction);
	}
}