package com.logicaldoc.web.service;

import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.DocumentLinkDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.web.AbstractWebappTCase;

import junit.framework.Assert;

@RunWith(MockitoJUnitRunner.class)
public class DocumentServiceImplTest extends AbstractWebappTCase {

	@Mock 
	private EMailSender emailSender;
	
	// Instance under test
	private DocumentServiceImpl service = new DocumentServiceImpl();

	private DocumentDAO docDao;

	private DocumentLinkDAO linkDao;

	private DocumentNoteDAO noteDao;

	private BookmarkDAO bookDao;

	private DocumentHistoryDAO documentHistoryDao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		docDao = (DocumentDAO) context.getBean("DocumentDAO");
		linkDao = (DocumentLinkDAO) context.getBean("DocumentLinkDAO");
		noteDao = (DocumentNoteDAO) context.getBean("DocumentNoteDAO");
		documentHistoryDao = (DocumentHistoryDAO) context.getBean("DocumentHistoryDAO");
		bookDao = (BookmarkDAO) context.getBean("BookmarkDAO");
		
		File emailFile = new File("target/tmp/logicaldoc/docs/5/doc/1.0");
		emailFile.getParentFile().mkdirs();

		// Copy sql files
		copyResource("/Joyce Jinks shared the Bruce Duo post.eml", emailFile.getCanonicalPath());		
	}

	@Test
	public void testGetVersionsById() throws ServerException {
		GUIVersion[] versions = service.getVersionsById(1, 2);
		Assert.assertNotNull(versions);
		Assert.assertEquals(2, versions.length);
	}

	@Test
	public void testGetById() throws ServerException {
		GUIDocument doc = service.getById(1);
		Assert.assertEquals(1, doc.getId());
		Assert.assertEquals("pippo", doc.getFileName());
		Assert.assertNotNull(doc.getFolder());
		Assert.assertEquals(5, doc.getFolder().getId());
		Assert.assertEquals("/", doc.getFolder().getName());

		doc = service.getById(3);
		Assert.assertEquals(3, doc.getId());
		Assert.assertEquals("pippo", doc.getFileName());

		// Try with unexisting document
		doc = service.getById(99);
		Assert.assertNull(doc);
	}

	@Test
	public void testSave() throws Exception {
		GUIDocument doc = service.getById(1);

		doc = service.save(doc);
		Assert.assertNotNull(doc);
		Assert.assertEquals("myself", doc.getPublisher());

		doc = service.getById(3);
		Assert.assertEquals("pippo", doc.getFileName());

		doc = service.save(doc);
		Assert.assertNotNull(doc);
	}

	@Test
	public void testUpdateLink() throws ServerException, PersistenceException {
		DocumentLink link = linkDao.findById(1);
		Assert.assertNotNull(link);
		Assert.assertEquals("test", link.getType());

		service.updateLink(1, "pippo");

		link = linkDao.findById(1);
		Assert.assertNotNull(link);
		Assert.assertEquals("pippo", link.getType());
	}

	@Test
	public void testDeleteLinks() throws ServerException, PersistenceException {
		DocumentLink link = linkDao.findById(1);
		Assert.assertNotNull(link);
		Assert.assertEquals("test", link.getType());
		link = linkDao.findById(2);
		Assert.assertNotNull(link);
		Assert.assertEquals("xyz", link.getType());

		service.deleteLinks(new long[] { 1, 2 });

		link = linkDao.findById(1);
		Assert.assertNull(link);
		link = linkDao.findById(2);
		Assert.assertNull(link);
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());
		doc = docDao.findById(2);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());
		Assert.assertEquals(1, doc.getDocRef().longValue());
		doc = docDao.findById(3);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());

		service.delete(new long[] { 2, 3 });

		doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		doc = docDao.findById(2);
		Assert.assertNull(doc);
		doc = docDao.findById(3);
		Assert.assertNull(doc);
	}

	@Test
	public void testDeleteNotes() throws ServerException {
		List<DocumentNote> notes = noteDao.findByDocId(1, "1.0");
		Assert.assertNotNull(notes);
		Assert.assertEquals(2, notes.size());
		Assert.assertEquals("message for note 1", notes.get(0).getMessage());

		service.deleteNotes(new long[] { 1 });

		notes = noteDao.findByDocId(1, "1.0");
		Assert.assertNotNull(notes);
		Assert.assertEquals(1, notes.size());
	}

	@Test
	public void testAddNote() throws ServerException, PersistenceException {
		List<DocumentNote> notes = noteDao.findByDocId(1L, "1.0");
		Assert.assertNotNull(notes);
		Assert.assertEquals(2, notes.size());

		long noteId = service.addNote(1L, "pippo");

		DocumentNote note = noteDao.findById(noteId);
		Assert.assertNotNull(note);
		Assert.assertEquals("pippo", note.getMessage());

		notes = noteDao.findByDocId(1L, "1.0");
		Assert.assertNotNull(notes);
		Assert.assertEquals(2, notes.size());
	}

	@Test
	public void testLock() throws ServerException, PersistenceException {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(3L, doc.getLockUserId().longValue());
		doc = docDao.findById(2);
		Assert.assertNotNull(doc);
		Assert.assertEquals(3L, doc.getLockUserId().longValue());

		service.unlock(new long[] { 1, 2 });

		doc = docDao.findDocument(1);
		Assert.assertNotNull(doc);
		Assert.assertNull(doc.getLockUserId());
		doc = docDao.findDocument(2);
		Assert.assertNotNull(doc);
		Assert.assertNull(doc.getLockUserId());

		service.lock(new long[] { 1, 2 }, "comment");

		doc = docDao.findDocument(1);
		Assert.assertEquals(1L, doc.getLockUserId().longValue());
		doc = docDao.findDocument(2);
		Assert.assertEquals(1L, doc.getLockUserId().longValue());
	}

	@Test
	public void testLinkDocuments() throws ServerException {
		service.linkDocuments(new long[] { 1, 2 }, new long[] { 3, 4 });

		DocumentLink link = linkDao.findByDocIdsAndType(1, 3, "default");
		Assert.assertNotNull(link);
		link = linkDao.findByDocIdsAndType(1, 4, "default");
		Assert.assertNotNull(link);
		link = linkDao.findByDocIdsAndType(2, 3, "default");
		Assert.assertNotNull(link);
		link = linkDao.findByDocIdsAndType(2, 4, "default");
		Assert.assertNotNull(link);
		link = linkDao.findByDocIdsAndType(3, 4, "default");
		Assert.assertNull(link);
	}

	@Test
	public void testRestore() throws ServerException, PersistenceException {
		docDao.delete(4);
		Assert.assertNull(docDao.findById(4));
		service.restore(new Long[] { 4L }, 5);
		Assert.assertNotNull(docDao.findById(4));
		Assert.assertNotNull(docDao.findById(4));
		Assert.assertEquals(5L, docDao.findById(4).getFolder().getId());
	}

	@Test
	public void testBookmarks() throws ServerException, PersistenceException {
		service.addBookmarks(new long[] { 1, 2 }, 0);

		Bookmark book = bookDao.findByUserIdAndDocId(1, 1);
		Assert.assertNotNull(book);
		book = bookDao.findByUserIdAndDocId(1, 2);
		Assert.assertNotNull(book);

		GUIBookmark bookmark = new GUIBookmark();
		bookmark.setId(book.getId());
		bookmark.setName("bookmarkTest");
		bookmark.setDescription("bookDescr");

		service.updateBookmark(bookmark);
		book = bookDao.findById(bookmark.getId());
		Assert.assertNotNull(book);
		Assert.assertEquals("bookmarkTest", book.getTitle());
		Assert.assertEquals("bookDescr", book.getDescription());

		service.deleteBookmarks(new long[] { bookmark.getId() });

		book = bookDao.findById(1);
		Assert.assertNull(book);
		book = bookDao.findById(2);
		Assert.assertNull(book);
	}

	@Test
	public void testMarkHistoryAsRead() throws ServerException {
		List<DocumentHistory> histories = documentHistoryDao.findByUserIdAndEvent(1, "data test 01", null);
		Assert.assertEquals(2, histories.size());
		Assert.assertEquals(1, histories.get(0).getIsNew());
		Assert.assertEquals(1, histories.get(1).getIsNew());

		service.markHistoryAsRead("data test 01");

		histories = documentHistoryDao.findByUserIdAndEvent(1, "data test 01", null);
		Assert.assertEquals(2, histories.size());
		Assert.assertEquals(0, histories.get(0).getIsNew());
		Assert.assertEquals(0, histories.get(1).getIsNew());
	}

	@Test
	public void testIndexable() throws ServerException, PersistenceException {
		Document doc1 = docDao.findById(1);
		Assert.assertNotNull(doc1);
		Assert.assertEquals(AbstractDocument.INDEX_INDEXED, doc1.getIndexed());
		Document doc2 = docDao.findById(2);
		Assert.assertNotNull(doc2);
		Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX, doc2.getIndexed());
		Document doc3 = docDao.findById(3);
		Assert.assertNotNull(doc3);
		Assert.assertEquals(AbstractDocument.INDEX_INDEXED, doc3.getIndexed());
		service.markUnindexable(new long[] { 1, 2, 3 });

		doc1 = docDao.findById(1);
		Assert.assertNotNull(doc1);
		Assert.assertEquals(AbstractDocument.INDEX_SKIP, doc1.getIndexed());
		doc2 = docDao.findById(2);
		Assert.assertNotNull(doc2);
		Assert.assertEquals(AbstractDocument.INDEX_SKIP, doc2.getIndexed());
		doc3 = docDao.findById(3);
		Assert.assertNotNull(doc3);
		Assert.assertEquals(AbstractDocument.INDEX_SKIP, doc3.getIndexed());

		service.markIndexable(new long[] { 1, 3 }, AbstractDocument.INDEX_TO_INDEX);

		doc1 = docDao.findById(1);
		Assert.assertNotNull(doc1);
		Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX, doc1.getIndexed());
		doc3 = docDao.findById(3);
		Assert.assertNotNull(doc3);
		Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX, doc3.getIndexed());
	}

	@Test
	public void testCountDocuments() throws ServerException {
		Assert.assertEquals(5, service.countDocuments(new long[] { 5 }, 0));
		Assert.assertEquals(0, service.countDocuments(new long[] { 5 }, 3));
	}
//	
//	@Test
//	public void testValidate() {
//		fail("Not yet implemented");
//	}

	@Test
	public void testSendAsEmail() throws Exception {
						
		emailSender = mock(EMailSender.class);
		
		service.setEmailSender(emailSender);
		
		doNothing().when(emailSender).send(any(EMail.class));
		
		try {						
			GUIEmail gmail = service.extractEmail(5, "1.0");
			System.err.println(gmail.getFrom().getEmail());
			gmail.setDocIds(new long[]{5});
			
			List<GUIContact> tos = new ArrayList<GUIContact>(); 
			GUIContact gc = new GUIContact("Kenneth", "Botterill", "ken-botterill@acme.com");
			tos.add(gc);
			
			GUIContact[] carr = new GUIContact[]{};
			gmail.setTos(tos.toArray(carr));			
			gmail.setBccs(null);
			gmail.setCcs(null);
			
			long tid = this.session.getUser().getTenant().getTenantId();
			long tid02 = this.session.getUser().getTenant().getId();
			
			System.err.println("tid:" + tid);
			System.err.println("tid02:" + tid02);
			
			String retmess = service.sendAsEmail(gmail, "en-US");
			System.err.println("returned message: " + retmess);
		} catch (ServerException e) {
			e.printStackTrace();
			
		}
	}
//
//	@Test
//	public void testGetNotes() {
//		fail("Not yet implemented");
//	}
//
//	@Test
//	public void testSaveNotes() {
//		fail("Not yet implemented");
//	}
//
//	@Test
//	public void testBulkUpdate() {
//		fail("Not yet implemented");
//	}

	@Test
	public void testExtractEmail() {
		try {
			service.extractEmail(4, null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}
	}

//	@Test
//	public void testSaveEmailAttachment() {
//		//fail("Not yet implemented");
//	}	
}