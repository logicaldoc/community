package com.logicaldoc.core.automation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Tenant;

public class DocToolTest extends AbstractCoreTestCase {

	// instance under test
	private DocTool testSubject = new DocTool();

	@Test
	public void testDownloadUrl() throws Exception {
		String url = testSubject.downloadUrl(1L);
		assertEquals("http://localhost:8080/download?docId=1", url);

		DocumentHistory hist = new DocumentHistory();
		hist.setDocId(2L);
		url = testSubject.downloadUrl(hist);
		assertEquals("http://localhost:8080/download?docId=2", url);
	}

	@Test
	public void testDisplayUrl() throws Exception {
		String url = testSubject.displayUrl(new Document());
		assertEquals("http://localhost:8080/display?tenant=default&docId=0", url);

		DocumentHistory hist = new DocumentHistory();
		hist.setDocId(2L);
		url = testSubject.displayUrl(hist);
		assertEquals("http://localhost:8080/display?tenant=default&docId=2", url);
	}

	@Test
	public void testDownloadTicket() throws Exception {
		String url = testSubject.downloadTicket(1L, true, 1, null, 3, "admin");
		assertTrue(url.startsWith("http://localhost:8080/download-ticket?ticketId="));
	}

	@Test
	public void testDisplayFileSize() throws Exception {
		final long kb = 1024L;
		String result = testSubject.displayFileSize(kb);
		assertEquals("1 KB", result);

		result = testSubject.displayFileSize(kb * kb);
		assertEquals("1 MB", result);

		result = testSubject.displayFileSize(kb * kb * kb);
		assertEquals("1 GB", result);

		result = testSubject.displayFileSize(1L);
		assertEquals("1 Bytes", result);
	}

	@Test
	public void testStore() throws Exception {
		Document doc = new Document();
		doc.setFileName("test.pdf");
		final Folder folder = new FolderTool().findById(4L);
		doc.setFolder(folder);

		DocumentHistory transaction = new DocumentHistory();
		transaction.setDocument(doc);
		transaction.setUserId(1L);
		transaction.setUsername("admin");
		transaction.setUserLogin("admin");

		testSubject.store(doc, transaction);
		assertNotSame(0L, doc.getId());

		doc = new Document();
		doc.setFileName("test2.pdf");
		doc.setFolder(folder);
		testSubject.store(doc);
		assertNotSame(0L, doc.getId());

		doc = new Document();
		doc.setFileName("test3.pdf");
		doc.setFolder(folder);
		testSubject.store(doc, "admin");
		assertNotSame(0L, doc.getId());
	}

	@Test
	public void testInitialize() throws Exception {
		TemplateDAO templateDao = (TemplateDAO) context.getBean("TemplateDAO");
		Template template = templateDao.findById(-1L);

		final Folder folder = new FolderTool().findById(4L);

		Document doc = new Document();
		doc.setFolder(folder);
		doc.setTemplate(template);
		doc.setFileName("pippo.pdf");

		doc.setValues("multi", List.of("value1", "value2", "value3"));
		testSubject.store(doc);
		assertNotSame(0L, doc.getId());

		doc = testSubject.findById(doc.getId());
		testSubject.initialize(doc);
		assertEquals(3, doc.getValues("multi").size());
		assertEquals("value1", doc.getValues("multi").get(0));
	}

	@Test
	public void testMove() throws Exception {
		FolderTool ft = new FolderTool();
		Folder testFolder = ft.createPath(ft.findById(4L), "/Default/test", "admin");
		Document doc = testSubject.findById(1L);
		assertNotNull(doc);
		testSubject.initialize(doc);
		testSubject.move(doc, "/Default/test", "admin");

		doc = testSubject.findByPath("/Default/test/pippo.pdf");
		assertEquals(1L, doc.getId());
		assertEquals(testFolder, doc.getFolder());
	}

	@Test
	public void testCopy() throws Exception {
		FolderTool ft = new FolderTool();
		Folder testFolder = ft.createPath(ft.findById(4L), "/Default/test", "admin");
		Document doc = testSubject.findById(1L);
		Document copied = testSubject.copy(doc, "/Default/test", "admin");
		assertEquals(testFolder, copied.getFolder());
		assertNotSame(doc, copied);
		assertNotSame(doc.getFolder(), copied.getFolder());

	}

	@Test
	public void testLink() throws Exception {
		Document doc1 = testSubject.findById(1);
		Document doc2 = testSubject.findById(2);
		DocumentLink link = testSubject.link(doc1, doc2, "test");
		assertNotNull(link);
		assertTrue(link.getId() > 0L);
	}

	@Test
	public void testCreateAlias() throws Exception {
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.initialize(doc);

		FolderTool folderTool = new FolderTool();
		Folder newFolder = folderTool.findById(6);
		folderTool.initialize(newFolder);

		Document alias = testSubject.createAlias(doc, newFolder, null, "admin");
		assertNotSame(doc.getId(), alias.getId());
		assertEquals(newFolder, alias.getFolder());
		assertEquals("pippo(1).pdf", alias.getFileName());

		alias = testSubject.createAlias(doc, folderTool.getPath(newFolder.getId()), null, "admin");
		assertNotSame(doc.getId(), alias.getId());
		assertEquals(newFolder, alias.getFolder());
		assertEquals("pippo(2).pdf", alias.getFileName());
	}

	@Test
	public void testLock() throws Exception {
		Document doc = testSubject.findById(1);
		assertNotNull(doc);
		testSubject.lock(doc.getId(), "admin");
		doc = testSubject.findById(1);
		assertEquals(2, doc.getStatus());
		assertEquals(1L, doc.getLockUserId().longValue());

		// double lock with same user just to check that no exceptions are
		// raised
		testSubject.lock(doc.getId(), "admin");
		testSubject.lock(doc.getId(), "admin");
	}

	@Test
	public void testUnlock() throws Exception {
		testSubject.lock(1L, "admin");

		Document doc = testSubject.findById(1L);
		assertEquals(2, doc.getStatus());
		assertEquals(1L, doc.getLockUserId().longValue());

		// Locked by a different user
		testSubject.unlock(doc.getId(), "admin");

		doc = testSubject.findById(1);
		assertEquals(0, doc.getStatus());
		assertNull(doc.getLockUserId());

		testSubject.unlock(doc.getId(), "admin");

		doc = testSubject.findById(1);
		assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
		assertNull(doc.getLockUserId());

		// Already unlocked
		testSubject.unlock(doc.getId(), "admin");
		doc = testSubject.findById(1);
		assertEquals(AbstractDocument.DOC_UNLOCKED, doc.getStatus());
		assertNull(doc.getLockUserId());
	}

	@Test
	public void testDelete() throws Exception {
		Document doc = testSubject.findById(1L);
		assertNotNull(doc);
		assertTrue(doc.getId() > 0L);

		testSubject.delete(1L, "admin");
		doc = testSubject.findById(1L);
		assertNull(doc);
	}

	@Test
	public void testCopyResource() throws Exception {
		Document doc = testSubject.findById(1L);
		Document result = testSubject.copyResource(doc, doc.getFileVersion(), "conversion.pdf", "xxx.pdf", "admin");
		assertNotNull(result);
		assertTrue(result.getId() > 0L);
		assertEquals("xxx.pdf", result.getFileName());
		assertTrue(result.getFileSize() > 0L);
	}

	@Test
	public void testReadAsString() throws Exception {
		Document doc = testSubject.findById(1L);
		String result = testSubject.readAsString(doc.getId(), doc.getFileVersion(), null);
		assertNotNull(result);
		assertTrue(result.contains("Linearized"));
	}

	@Test
	public void testWriteToFile() throws Exception {
		Document doc = testSubject.findById(1L);
		testSubject.writeToFile(1L, doc.getFileVersion(), "conversion.pdf", "target/test.pdf");
		File extraction = new File("target/test.pdf");
		assertTrue(extraction.exists());
		assertTrue(extraction.length() > 0);
		extraction.delete();
	}

	@Test
	public void testConvert() throws Exception {
		Document doc = testSubject.findById(1L);
		testSubject.convertPDF(doc);
		Document result = testSubject.convert(doc, "pdf", "admin");
		assertNotNull(result);
	}

	@Test
	public void testMerge() throws Exception {
		Document doc1 = testSubject.findById(1);
		assertNotNull(doc1);
		testSubject.initialize(doc1);
		assertEquals(55, testSubject.countPages(doc1));

		Document doc3 = testSubject.findById(3);
		assertNotNull(doc3);
		testSubject.initialize(doc3);
		assertEquals(1, testSubject.countPages(doc3));

		Document mergedDoc = testSubject.merge(Arrays.asList(doc1, doc3), 1200L, "merged.pdf", "admin");
		assertNotNull(mergedDoc);

		mergedDoc = testSubject.findById(mergedDoc.getId());
		assertNotNull(mergedDoc);
		testSubject.initialize(mergedDoc);

		assertEquals(56, testSubject.countPages(mergedDoc));
	}

	@Test
	public void testFindByPath() throws Exception {
		Document doc = testSubject.findById(1L);
		String path = testSubject.getPath(doc);
		Document pathDoc = testSubject.findByPath(path);
		assertEquals(doc, pathDoc);
	}

	@Test
	public void testGetPath() throws Exception {
		String result = testSubject.getPath(testSubject.findById(1L));
		assertEquals("/Workspace X/folder6/pippo.pdf", result);
	}

	@Test
	public void testGetIds() throws Exception {
		Document doc1 = new Document();
		doc1.setId(101L);
		Document doc2 = new Document();
		doc2.setId(102L);
		Document doc3 = new Document();
		doc3.setId(103L);
		List<Long> ids = testSubject.getIds(Arrays.asList(doc1, doc2, doc3));
		assertEquals(3, ids.size());
		assertEquals(Long.valueOf(102L), ids.get(1));
	}

	@Test
	public void testCreatePath() throws Exception {
		Document doc = testSubject.findById(1L);
		Folder result = testSubject.createPath(doc, "/Default/xxx", "admin");
		FolderTool ft = new FolderTool();
		assertEquals(result, ft.findByPath("/Default/xxx", doc.getTenantId()));

		result = testSubject.createPath(doc, "yyy", "admin");
		assertEquals(result, ft.findByPath("/Workspace X/folder6/yyy", doc.getTenantId()));

		result = testSubject.createPath(doc, "/zzz", "admin");
		assertEquals(result, ft.findByPath("/Default/zzz", doc.getTenantId()));
	}

	@Test
	public void testGetHistories() throws Exception {
		List<DocumentHistory> result = testSubject.getHistories(1L, null);
		assertEquals(3, result.size());

		result = testSubject.getHistories(1L, DocumentEvent.STORED.toString());
		assertEquals(0, result.size());
	}

	@Test
	public void testAddNote() throws Exception {
		Document doc = testSubject.findById(1L);
		assertEquals(2, testSubject.getNotes(1L, null).size());

		testSubject.addNote(doc, "my note", "admin");
		assertEquals(3, testSubject.getNotes(1L, null).size());
		assertEquals(3, testSubject.getNotes(1L, doc.getFileVersion()).size());
	}

	@Test
	public void testCalculateNextVersion() throws Exception {
		String result = testSubject.calculateNextVersion("1.1", true);
		assertEquals("2.0", result);
		result = testSubject.calculateNextVersion("1.1", false);
		assertEquals("1.2", result);
	}

	@Test
	public void testFindTemplateByName() throws Exception {
		Template template = testSubject.findTemplateByName("email", Tenant.DEFAULT_ID);
		assertNotNull(template);
		assertEquals("email", template.getName());

		template = testSubject.findTemplateByName("unexisting", Tenant.DEFAULT_ID);
		assertNull(template);
	}

	@Test
	public void testFindTemplateById() throws Exception {
		Template template = testSubject.findTemplateById(-1L);
		assertNotNull(template);
		assertEquals("default", template.getName());

		template = testSubject.findTemplateById(999L);
		assertNull(template);
	}

	@Test
	public void testCountPages() throws Exception {
		Document doc = testSubject.findById(1L);
		assertEquals(55, testSubject.countPages(doc));
	}

	@Test
	public void testParse() throws Exception {
		Document doc = testSubject.findById(1);
		String text = testSubject.parse(doc, doc.getFileVersion());
		assertTrue(text.contains("Digital Day"));
	}
}