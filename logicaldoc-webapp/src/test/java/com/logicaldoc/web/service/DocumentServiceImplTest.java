package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.web.AbstractWebappTCase;

import junit.framework.Assert;

@RunWith(MockitoJUnitRunner.class)
public class DocumentServiceImplTest extends AbstractWebappTCase {

	private static Logger log = LoggerFactory.getLogger(DocumentServiceImplTest.class);

	@Mock
	private EMailSender emailSender;

	// Instance under test
	private DocumentServiceImpl service = new DocumentServiceImpl();

	private DocumentDAO docDao;

	private TemplateDAO templateDao;

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
		templateDao = (TemplateDAO) context.getBean("TemplateDAO");

		File emailFile = new File("target/tmp/logicaldoc/docs/5/doc/1.0");
		emailFile.getParentFile().mkdirs();
		// Copy email file
		copyResource("/Joyce Jinks shared the Bruce Duo post.eml", emailFile.getCanonicalPath());

		emailFile = new File("target/tmp/logicaldoc/docs/6/doc/1.0");
		emailFile.getParentFile().mkdirs();
		// Copy email file
		copyResource("/Hurry up! Only a few hours for the Prime Day VGA promos !!!.msg", emailFile.getCanonicalPath());

		emailFile = new File("target/tmp/logicaldoc/docs/7/doc/1.0");
		emailFile.getParentFile().mkdirs();
		// Copy email file
		copyResource("/New error indexing documents.eml", emailFile.getCanonicalPath());
	}

	@Test
	public void testIndex() throws ServerException, IOException {
		GUIDocument doc = service.getById(7);
		doc.setIndexed(0);
		service.save(doc);
		
		doc = service.getById(7);
		Assert.assertEquals(0, doc.getIndexed());
		
		copyResource("/New error indexing documents.eml", "target/tmp/logicaldoc/docs/7/doc/1.1");
		
		service.indexDocuments(new Long[]{doc.getId()});
		doc = service.getById(1);
		Assert.assertEquals(1, doc.getIndexed());
	}
	
	@Test
	public void testGetVersionsById() throws ServerException {
		GUIVersion[] versions = service.getVersionsById(1, 2);
		Assert.assertNotNull(versions);
		Assert.assertEquals(2, versions.length);

		// only the first version of the two
		versions = service.getVersionsById(1, 23);
		Assert.assertNotNull(versions);
		Assert.assertEquals(1, versions.length);

		// only the 2nd version of the two
		versions = service.getVersionsById(21, 2);
		Assert.assertNotNull(versions);
		Assert.assertEquals(1, versions.length);

		// no versions
		versions = service.getVersionsById(21, 22);
		Assert.assertNotNull(versions);
		Assert.assertEquals(0, versions.length);
	}

	@Test
	public void testDeleteVersions() throws ServerException {
		long[] ids = new long[] { 21, 22, 23 };
		GUIDocument gdoc;
		boolean exceptionHappened = false;
		try {
			gdoc = service.deleteVersions(ids);
		} catch (AssertionError | ServerException e) {
			exceptionHappened = true;
		}
		Assert.assertTrue(exceptionHappened);

		ids = new long[] { 1, 2 };
		gdoc = service.deleteVersions(ids);
		assertNotNull(gdoc);
		assertEquals(1, gdoc.getId());
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

		doc = docDao.findById(1);
		Assert.assertNotNull(doc);
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

		try {
			// add note to a non existent doc
			service.addNote(21L, "Midnight Rain");
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}
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

		// delete an already deleted bookmark
		service.deleteBookmarks(new long[] { bookmark.getId() });

		// Add bookmarks on folders
		service.addBookmarks(new long[] { 6, 7 }, Bookmark.TYPE_FOLDER);

		// Add bookmarks on non existent documents
		try {
			service.addBookmarks(new long[] { 21, 22 }, Bookmark.TYPE_DOCUMENT);
			fail("Expected exception was not thrown");
		} catch (Throwable e) {
			e.printStackTrace();
			// nothing to do
		}
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
		Assert.assertEquals(7, service.countDocuments(new long[] { 5 }, 0));
		Assert.assertEquals(0, service.countDocuments(new long[] { 5 }, 3));
	}

	@Test
	public void testValidate() throws PersistenceException, ServerException {
		/*
		 * validate a simple document (no template assigned)
		 */
		GUIDocument gdoc = service.getById(1);
		service.validate(gdoc);

		// Update the document add a template
		Document doc = docDao.findDocument(6);
		docDao.initialize(doc);

		Template template = templateDao.findById(5L);
		templateDao.initialize(template);

		// Set the validator for attribute "attr1" to be email format
		template.getAttribute("attr1").setValidation(
				"#if(!$value.matches('^([\\w-\\.]+){1,64}@([\\w&&[^_]]+){2,255}.[a-z]{2,}$')) $error.setDescription($I18N.get('invalidformat')); #end");
		templateDao.store(template);

		doc.setTemplate(template);
		docDao.store(doc);

		gdoc = service.getById(6);

		/*
		 * validate a document with template assigned
		 */

		// // The value of attribute "attr1" is: "val1" so this should produce
		// an error

		try {
			service.validate(gdoc);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			String lcal = I18N.message("invalidformat");
			assertTrue(e.getMessage().contains("attr1"));
			assertTrue(e.getMessage().contains(lcal));
		}

		/*
		 * validate a document with template assigned
		 */

		// Update the document add a template
		doc = docDao.findDocument(6);
		docDao.initialize(doc);

		// update the attribute and set the value as an email format
		Attribute xxx = new Attribute();
		xxx.setValue("test.xx@acme.de");
		doc.setAttribute("attr1", xxx);
		docDao.store(doc);

		gdoc = service.getById(6);

		// The value of attribute "attr1" is "test.xx@acme.de" this will
		// validate correctly
		service.validate(gdoc);
	}

	@Test
	public void testSendAsEmail() throws Exception {

		emailSender = mock(EMailSender.class);

		DocumentServiceImpl.setEmailSender(emailSender);

		doNothing().when(emailSender).send(any(EMail.class));

		// Send the email as download ticket
		try {
			GUIEmail gmail = service.extractEmail(5, "1.0");
			log.info(gmail.getFrom().getEmail());
			gmail.setDocIds(new long[] { 5 });

			List<GUIContact> tos = new ArrayList<GUIContact>();
			GUIContact gc = new GUIContact("Kenneth", "Botterill", "ken-botterill@acme.com");
			tos.add(gc);

			GUIContact[] carr = new GUIContact[] {};
			gmail.setTos(tos.toArray(carr));

			tos = new ArrayList<GUIContact>();
			gc = new GUIContact("Riley", "Arnold", "riley-arnold@acme.com");
			tos.add(gc);
			gmail.setBccs(tos.toArray(carr));

			tos = new ArrayList<GUIContact>();
			gc = new GUIContact("Scout", "Marsh", "s.marsh@acme.com");
			tos.add(gc);
			gmail.setCcs(tos.toArray(carr));
			gmail.setSendAsTicket(true);

			long tid = this.session.getUser().getTenant().getTenantId();
			long tid02 = this.session.getUser().getTenant().getId();

			log.info("tid: {}", tid);
			log.info("tid02: {}", tid02);

			String retvalue = service.sendAsEmail(gmail, "en-US");
			log.info("returned message: {}", retvalue);
			assertEquals("ok", retvalue);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}

		// Send the email in .zip compressed form
		try {
			GUIEmail gmail = service.extractEmail(5, "1.0");
			log.info(gmail.getFrom().getEmail());
			gmail.setDocIds(new long[] { 5 });

			List<GUIContact> tos = new ArrayList<GUIContact>();
			GUIContact gc = new GUIContact("Kenneth", "Botterill", "ken-botterill@acme.com");
			tos.add(gc);

			GUIContact[] carr = new GUIContact[] {};
			gmail.setTos(tos.toArray(carr));
			gmail.setBccs(null);
			gmail.setCcs(null);

			gmail.setSendAsTicket(false);
			gmail.setZipCompression(true);

			String retvalue = service.sendAsEmail(gmail, "en-US");
			log.info("returned message: {}", retvalue);
			assertEquals("ok", retvalue);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testGetNotes() {

		// test on a non existent doc
		try {
			service.getNotes(600, "1.0", null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// test on a doc without notes
		try {
			GUIDocumentNote[] notes = service.getNotes(6, "1.0", null);
			assertEquals(0, notes.length);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}

		// get a document with a single note
		try {
			GUIDocumentNote[] notes = service.getNotes(4, "1.0", null);
			assertEquals(1, notes.length);

			notes = service.getNotes(4, null, null);
			assertEquals(1, notes.length);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testSaveNotes() {
		try {
			List<GUIDocumentNote> notes = new ArrayList<>();
			service.saveNotes(888, notes.toArray(new GUIDocumentNote[] {}), null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		try {
			List<GUIDocumentNote> notes = new ArrayList<>();
			GUIDocumentNote gdn01 = new GUIDocumentNote();
			gdn01.setDocId(5);
			gdn01.setMessage("Vigilante Shit");
			GUIDocumentNote gdn02 = new GUIDocumentNote();
			gdn02.setDocId(5);
			gdn02.setMessage("Karma");
			gdn02.setRecipient("Kenneth Botterill");
			gdn02.setRecipientEmail("ken-botterill@acme.com");
			notes.add(gdn01);
			notes.add(gdn02);
			service.saveNotes(5, notes.toArray(new GUIDocumentNote[] {}), null);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testBulkUpdate() throws ParseException, PersistenceException {

		long[] ids = new long[] { 2, 3, 4 };
		GUIDocument vo = new GUIDocument();
		vo.setPublished(1);

		String sDate1 = "10-21-2022";
		Date date1 = new SimpleDateFormat("MM-dd-yyyy").parse(sDate1);
		String sDate2 = "12-31-2089";
		Date date2 = new SimpleDateFormat("MM-dd-yyyy").parse(sDate2);

		vo.setStartPublishing(date1);
		vo.setStopPublishing(date2);
		vo.setLanguage("en");
		vo.setTags(new String[] { "Maroon", "Anti-Hero", "Karma" });
		vo.setTemplateId(5L);

		// set attributes
		GUIAttribute[] attributes = new GUIAttribute[1];
		GUIAttribute gat = new GUIAttribute();
		gat.setName("attr1");
		gat.setType(0);
		gat.setStringValue("Snow on the Beach");

		attributes[0] = gat;
		vo.setAttributes(attributes);

		try {
			GUIDocument[] gdocs = service.bulkUpdate(ids, vo, true);
			assertNotNull(gdocs);
			assertTrue(gdocs.length > 0);
			assertNotNull(gdocs[0].getTags());
			assertTrue(gdocs[0].getTags().length == 3);

			GUIAttribute gatX = gdocs[0].getAttribute("attr1");
			assertNotNull(gatX);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}

		// Test with a doc locked

		Document doc = docDao.findDocument(5);
		docDao.initialize(doc);
		doc.setStatus(AbstractDocument.DOC_CHECKED_OUT);
		docDao.store(doc);

		ids = new long[] { 5, 6 };
		vo = new GUIDocument();
		vo.setPublished(0);

		try {
			GUIDocument[] gdocs = service.bulkUpdate(ids, vo, true);
			assertNotNull(gdocs);
			assertTrue(gdocs.length > 0);

			// only one document updated because 1 was locked (checked-out)
			assertEquals(1, gdocs.length);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExtractEmail() {

		// test with document that is not an email (wrong or no extension)
		try {
			service.extractEmail(4, null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// test with document that is a .msg email file
		try {
			service.extractEmail(6, null);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testSaveEmailAttachment() {

		// try to extract an attachment from a non email document
		try {
			service.saveEmailAttachment(4, "testDocVer", "data.sql");
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// try to save an attachment that is not present in the document
		try {
			service.saveEmailAttachment(5, "1.0", "data.sql");
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// try to save an attachment that is not present in the document
		try {
			service.saveEmailAttachment(7, "1.0", "2022-01-04_15h54_11.png");
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

}