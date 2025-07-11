package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import jakarta.activation.DataHandler;
import jakarta.activation.FileDataSource;
import jakarta.mail.MessagingException;

import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSLink;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSUtil;

/**
 * Test case for <code>SoapDocumentService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapDocumentServiceTest extends AbstractWebserviceTestCase {

	private DocumentDAO docDao;

	private FolderDAO folderDao;

	// Instance under test
	private SoapDocumentService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		docDao = Context.get(DocumentDAO.class);
		folderDao = Context.get(FolderDAO.class);

		// Make sure that this is a DocumentServiceImpl instance
		testSubject = new SoapDocumentService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testUpdate() throws AuthenticationException, PermissionException, PersistenceException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals(103, doc.getFolder().getId());
		Document newDoc = docDao.findById(2);
		assertNotNull(newDoc);
		assertEquals(103, newDoc.getFolder().getId());
		docDao.initialize(doc);
		docDao.initialize(newDoc);

		WSDocument wsDoc = WSUtil.toWSDocument(newDoc);
		assertEquals(2, wsDoc.getId());
		wsDoc.setId(1);
		wsDoc.setCustomId("xxxxxxxx");
		assertEquals(1, wsDoc.getId());
		assertEquals("pluto", wsDoc.getFileName());

		testSubject.update("", wsDoc);

		docDao.initialize(doc);
		assertEquals("pluto(1)", doc.getFileName());
	}

	@Test
	public void testAddNote() throws AuthenticationException, PersistenceException, PermissionException,
	        UnexistingResourceException, WebserviceException {

	    Document doc = docDao.findById(1);
	    assertNotNull(doc);

	    List<WSNote> initialNotes = testSubject.getNotes("", doc.getId());
	    int originalNoteCount = initialNotes.size();

	    testSubject.addNote("", doc.getId(), "note1");
	    WSNote note = testSubject.addNote("", doc.getId(), "note2");
	    List<WSNote> notes = testSubject.getNotes("", doc.getId());
	    assertEquals(originalNoteCount + 2, notes.size());

	    testSubject.deleteNote("", note.getId());

	    notes = testSubject.getNotes("", doc.getId());
	    assertEquals(originalNoteCount + 1, notes.size());
	}

	@Test
	public void testRateDocument() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);

		WSRating rating = testSubject.rateDocument("", doc.getId(), 3);
		assertNotNull(rating);
		assertEquals(3, rating.getVote());

		List<WSRating> ratings = testSubject.getRatings("", doc.getId());
		assertEquals(1, ratings.size());
	}

	@Test
	public void testMove() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		Folder folder = doc.getFolder();
		assertEquals(103, folder.getId());

		Folder newFolder = folderDao.findById(80);
		docDao.initialize(doc);
		doc.setIndexingStatus(0);
		docDao.store(doc);
		testSubject.move("", doc.getId(), newFolder.getId());
		// NOTA: attenzione errore optimistic lock
		assertSame(1L, doc.getId());
		docDao.initialize(doc);
		assertEquals(newFolder, doc.getFolder());
	}

	@Test
	public void testLock() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		testSubject.lock("", 1);

		Document doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);
		assertEquals(DocumentStatus.LOCKED, doc.getStatus());
		assertEquals(1L, doc.getLockUserId().longValue());

		testSubject.unlock("", 1);
	}

	@Test
	public void testGetrecentDocuments() throws AuthenticationException, PersistenceException, WebserviceException {
		List<WSDocument> documents = testSubject.getRecentDocuments("", 10);
		assertEquals(2, documents.size());
	}

	@Test
	public void testSendEmail() throws AuthenticationException, PersistenceException, WebserviceException, IOException,
			MessagingException, PermissionException {
		WSDocument wsDoc = createDocument();

		DocumentHistoryDAO hDao = Context.get(DocumentHistoryDAO.class);
		long sentCount = hDao.findByDocId(wsDoc.getId()).stream()
				.filter(h -> DocumentEvent.SENT.toString().equals(h.getEvent())).count();
		assertEquals(0L, sentCount);
		try {
			testSubject.sendEmail(session.getSid(), List.of(wsDoc.getId()), "test@acme.com", "test", "test");
			sentCount = hDao.findByDocId(wsDoc.getId()).stream()
					.filter(h -> DocumentEvent.SENT.toString().equals(h.getEvent())).count();
			assertEquals(1L, sentCount);
		} catch (Exception e) {
			fail(e.getMessage());
		}

	}

	@Test
	public void testCreateAlias() throws PermissionException, PersistenceException, IOException, WebserviceException {
		WSDocument wsDoc = createDocument();
		WSDocument alias = testSubject.createAlias(session.getSid(), wsDoc.getId(), 80L, "role");
		assertNotNull(alias.getDocRef());
		assertEquals(wsDoc.getId(), alias.getDocRef().longValue());
	}

	@Test
	public void testReindex() throws AuthenticationException, PersistenceException, ParsingException,
			WebserviceException, PermissionException, IOException {
		WSDocument wsDoc = createDocument();
		testSubject.reindex(session.getSid(), wsDoc.getId(), "paper");
		assertEquals("paper", testSubject.getExtractedText(session.getSid(), wsDoc.getId()));
	}

	@Test
	public void testCreateTicket()
			throws AuthenticationException, PersistenceException, PermissionException, WebserviceException {
		assertNotNull(testSubject.createDownloadTicket(session.getSid(), 1L, "https://localhost:8080", 12, null, 100));
		assertNotNull(testSubject.createViewTicket(session.getSid(), 1L, "https://localhost:8080", 12, null, 100, 20));
	}

	@Test
	public void testDeleteVersion() throws PermissionException, PersistenceException, IOException, WebserviceException,
			AuthenticationException, UnexistingResourceException {
		WSDocument wsDoc = createDocument();
		testSubject.checkout(session.getSid(), wsDoc.getId());
		testSubject.checkin(session.getSid(), wsDoc.getId(), "comment", "pom.xml", true,
				new DataHandler(new FileDataSource(new File("pom.xml"))));

		WSDocument version = null;
		while (version == null)
			version = testSubject.getVersion(session.getSid(), wsDoc.getId(), "2.0");

		assertEquals(2, testSubject.getVersions(session.getSid(), wsDoc.getId()).size());

		testSubject.deleteVersion(session.getSid(), wsDoc.getId(), "2.0");
		assertEquals(1, testSubject.getVersions(session.getSid(), wsDoc.getId()).size());
	}

	@Test
	public void testPromoteVersion() throws PermissionException, PersistenceException, IOException, WebserviceException,
			AuthenticationException, UnexistingResourceException {
		WSDocument wsDoc = createDocument();
		testSubject.checkout(session.getSid(), wsDoc.getId());
		testSubject.checkin(session.getSid(), wsDoc.getId(), "comment", "build.xml", true,
				new DataHandler(new FileDataSource(new File("build.xml"))));

		WSDocument version = null;
		while (version == null)
			version = testSubject.getVersion(session.getSid(), wsDoc.getId(), "2.0");

		testSubject.promoteVersion("2.0", wsDoc.getId(), "2.0");

		DataHandler handler = testSubject.getContent(session.getSid(), wsDoc.getId());

		File originalFile = new File("build.xml");
		File tmp = null;
		try {
		    tmp = FileUtil.createTempFile("test", ".xml");
		    handler.writeTo(new FileOutputStream(tmp));
		    assertEquals(originalFile.length(), tmp.length());
		} finally {
		    FileUtil.delete(tmp);
		}
	}

	@Test
	public void testDocumentByCustomId() throws PermissionException, PersistenceException, IOException,
			WebserviceException, AuthenticationException {
		WSDocument wsDoc = createDocument();

		WSDocument doc = testSubject.getDocumentByCustomId(session.getSid(), "yyyyyyyy");
		assertNotNull(doc);
		assertEquals(wsDoc.getId(), doc.getId());
	}

	@Test
	public void testReplaceFile() throws PermissionException, PersistenceException, IOException, WebserviceException,
			AuthenticationException, UnexistingResourceException {
		WSDocument wsDoc = createDocument();

		File file = new File("build.xml");
		testSubject.replaceFile(session.getSid(), wsDoc.getId(), "1.0", "comment",
				new DataHandler(new FileDataSource(file)));

		DataHandler handler = testSubject.getContent(session.getSid(), wsDoc.getId());

		File tmp = null;
		try {
			tmp = FileUtil.createTempFile("test", ".xml");
			handler.writeTo(new FileOutputStream(tmp));
			assertEquals(file.length(), tmp.length());
		} finally {
			FileUtil.delete(tmp);
		}
	}

	@Test
	public void testSaveNote() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		WSNote note = new WSNote();
		note.setMessage("new note");
		note.setPage(1);

		note = testSubject.saveNote(session.getSid(), 1L, note);
		assertNotNull(note);
		assertEquals(1L, note.getDocId());
	}

	@Test
	public void testLink() throws AuthenticationException, PersistenceException, PermissionException,
			WebserviceException, UnexistingResourceException, IOException {
		List<WSLink> links = testSubject.getLinks(session.getSid(), 1L);
		assertEquals(4, links.size());

		WSDocument wsDoc = createDocument();
		WSLink link = testSubject.link(session.getSid(), 1L, wsDoc.getId(), "role");
		assertNotNull(link);
		assertEquals(1L, link.getDoc1());
		assertEquals(wsDoc.getId(), link.getDoc2());

		links = testSubject.getLinks(session.getSid(), 1L);
		assertEquals(5, links.size());

		testSubject.deleteLink(session.getSid(), wsDoc.getId());

		links = testSubject.getLinks(session.getSid(), 1L);
		assertEquals(4, links.size());
	}

	@Test
	public void testCreate() throws PermissionException, PersistenceException, IOException, WebserviceException,
			AuthenticationException, UnexistingResourceException {
		WSDocument wsDoc = createDocument();

		Document doc = docDao.findByFileNameAndParentFolderId(wsDoc.getFolderId(), wsDoc.getFileName(), null, 1L, null)
				.get(0);

		assertNotNull(doc);
		docDao.initialize(doc);

		assertEquals("document test.txt", doc.getFileName());
		assertEquals("coverage-val", doc.getValue("coverage"));

		wsDoc = testSubject.getDocument("xxxx", doc.getId());
		assertEquals("document test.txt", wsDoc.getFileName());
		assertEquals("coverage-val", wsDoc.getAttribute("coverage").getStringValue());
	}

	private WSDocument createDocument()
			throws IOException, PermissionException, WebserviceException, PersistenceException {
		WSDocument wsDoc = new WSDocument();
		wsDoc.setId(0L);
		wsDoc.setFolderId(4L);
		wsDoc.setTemplateId(-1L);
		wsDoc.setFileName("document test.txt");
		wsDoc.setCustomId("yyyyyyyy");
		File file = new File("pom.xml");
		wsDoc.setComment("comment");
		WSAttribute att = new WSAttribute();
		att.setName("coverage");
		att.setStringValue("coverage-val");
		wsDoc.addAttribute(att);
		return testSubject.create("xxxx", wsDoc, new DataHandler(new FileDataSource(file)));
	}

	@Test
	public void testUpload() throws AuthenticationException, PersistenceException, PermissionException,
			WebserviceException, IOException {
		File file = new File("pom.xml");
		long docId = testSubject.upload("xxxx", null, 4L, true, "document test.txt", "en",
				new DataHandler(new FileDataSource(file)));
		assertTrue(docId > 0L);

		long docId2 = testSubject.upload("xxxx", docId, null, true, "document test.txt", "en",
				new DataHandler(new FileDataSource(file)));

		assertEquals(docId, docId2);
		assertEquals("2.0", docDao.findById(docId2).getVersion());
	}

	@Test
	public void testCheckin() throws AuthenticationException, PersistenceException, PermissionException,
			WebserviceException, IOException {
		testSubject.checkout("", 1);

		Document doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);
		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

		File file = new File("pom.xml");
		testSubject.checkin("", 1, "comment", "pom.xml", true, new DataHandler(new FileDataSource(file)));

		doc = docDao.findById(1);
		assertNotNull(doc);
		docDao.initialize(doc);

		assertEquals(0, doc.getSigned());
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
	}

	@Test
	public void testGetContent() throws AuthenticationException, PersistenceException, PermissionException,
			WebserviceException, IOException, InterruptedException {
		WSDocument wsDoc = createDocument();

		DataHandler dh = testSubject.getContent("", wsDoc.getId());
		assertNotNull(dh);
		assertEquals("text/plain", dh.getContentType());

		WSDocument version = null;
		while (version == null) {
			try {
				version = testSubject.getVersion("xxxx", wsDoc.getId(), wsDoc.getVersion());
			} catch (UnexistingResourceException e) {
				// Continue
			}
		}

		dh = testSubject.getVersionContent("", wsDoc.getId(), wsDoc.getVersion());
		assertNotNull(dh);
		assertEquals("text/plain", dh.getContentType());
	}

	@Test
	public void testAccessControlList()
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {

		List<WSAccessControlEntry> acl = new ArrayList<>();
		WSAccessControlEntry ace = new WSAccessControlEntry();
		ace.setGroupId(2L);
		ace.setRead(1);
		ace.setWrite(1);
		acl.add(ace);

		ace = new WSAccessControlEntry();
		ace.setGroupId(3L);
		ace.setRead(1);
		ace.setWrite(0);
		acl.add(ace);

		testSubject.setAccessControlList(session.getSid(), 1L, acl);

		acl = testSubject.getAccessControlList(session.getSid(), 1L);
		assertEquals(2, acl.size());
		assertTrue(acl.stream().anyMatch(a -> a.getGroupId() == 2L && a.getWrite() == 1));
		assertTrue(acl.stream().anyMatch(a -> a.getGroupId() == 3L && a.getWrite() == 0));
	}

	@Test
	public void testUploadResource()
			throws PermissionException, PersistenceException, IOException, WebserviceException {
		WSDocument wsDoc = createDocument();

		testSubject.uploadResource("", wsDoc.getId(), wsDoc.getFileVersion(), "test",
				new DataHandler(new FileDataSource(new File("pom.xml"))));
		DataHandler dh = testSubject.getResource("", wsDoc.getId(), wsDoc.getFileVersion(), "test");
		assertNotNull(dh);
		assertEquals("application/octet-stream", dh.getContentType());
	}

	@Test
	public void testCopy() throws PermissionException, PersistenceException, IOException, WebserviceException {
		WSDocument wsDoc = createDocument();
		wsDoc = testSubject.copy(null, wsDoc.getId(), 80L, true, true, true);
		assertEquals(80L, wsDoc.getFolderId().longValue());
	}

	@Test
	public void testDelete()
			throws PersistenceException, AuthenticationException, PermissionException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		testSubject.delete("", doc.getId());
		doc = docDao.findById(1);
		assertNull(doc);
	}

	@Test
	public void testRename() throws PersistenceException, AuthenticationException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals("pippo", doc.getFileName());
		docDao.initialize(doc);
		testSubject.rename("", 1, "pippo");
		docDao.initialize(doc);
		assertEquals("pippo", doc.getFileName());
	}

	@Test
	public void testGetDocument() throws PersistenceException, AuthenticationException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);

		WSDocument wsDoc = testSubject.getDocument("", 1);

		assertEquals(1, wsDoc.getId());
		assertEquals("pippo", wsDoc.getFileName());
		assertEquals(103, wsDoc.getFolderId().longValue());
	}

	@Test
	public void testGetDocumentByCustomId() throws PersistenceException, AuthenticationException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Document doc = docDao.findByCustomId("a", Tenant.DEFAULT_ID);
		assertNotNull(doc);

		WSDocument wsDoc = testSubject.getDocument("", 1);

		assertEquals(1, wsDoc.getId());
		assertEquals("pippo", wsDoc.getFileName());
		assertEquals(103, wsDoc.getFolderId().longValue());
	}

	@Test
	public void testIsRead() throws AuthenticationException, PersistenceException, WebserviceException {
		assertTrue(testSubject.isRead("", 1));
	}

	@Test
	public void testRestore() throws PersistenceException, AuthenticationException, WebserviceException {
		assertNull(docDao.findById(4));
		testSubject.restore("", 4, 5);
		assertNotNull(docDao.findById(4));
	}

	@Test
	public void testGetVersions() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		List<WSDocument> versions = testSubject.getVersions("", 1);
		assertEquals(2, versions.size());
		assertEquals("testVer02", versions.get(0).getVersion());
		assertEquals("testVer01", versions.get(1).getVersion());

		versions = testSubject.getVersions("", 2L);
		assertEquals(2, versions.size());
	}

	@Test
	public void testListDocuments()
			throws AuthenticationException, PersistenceException, PermissionException, WebserviceException {
		List<WSDocument> docs = testSubject.listDocuments("", 103, null);
		assertNotNull(docs);
		assertEquals(2, docs.size());
		assertEquals(1, docs.get(0).getId());
		assertEquals(2, docs.get(1).getId());

		docs = testSubject.listDocuments("", 103, "plo");
		assertNotNull(docs);
		assertEquals(0, docs.size());

		docs = testSubject.listDocuments("", 103, "*ut*");
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals("pluto", docs.get(0).getFileName());

		docs = testSubject.listDocuments("", 103, "pippo");
		assertNotNull(docs);
		assertEquals(1, docs.size());
		assertEquals("pippo", docs.get(0).getFileName());
	}

	@Test
	public void testGetDocuments() throws AuthenticationException, PersistenceException, WebserviceException {
		List<WSDocument> docs = testSubject.getDocuments("", List.of(1L, 2L, 3L));
		assertNotNull(docs);
		assertEquals(2, docs.size());
	}

	@Test
	public void testGetAliases() throws AuthenticationException, PersistenceException, WebserviceException {
		List<WSDocument> docs = testSubject.getAliases("", 1L);
		assertNotNull(docs);
		assertEquals(1, docs.size());

		docs = testSubject.getAliases("", 2L);
		assertNotNull(docs);
		assertEquals(0, docs.size());
	}

	@Test
	public void testSetPassword() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Session session = SessionManager.get().newSession("admin", "admin", (Client) null);
		testSubject.setPassword(session.getSid(), 1L, "test");

		try {
			testSubject.unsetPassword(session.getSid(), 1L, "adsfddf");
		} catch (Exception t) {
			assertNotNull(t);
		}

		testSubject.unsetPassword(session.getSid(), 1L, "test");
	}

	@Test
	public void testUnprotect() throws AuthenticationException, PersistenceException, PermissionException,
			UnexistingResourceException, WebserviceException {
		Session session = SessionManager.get().newSession("admin", "admin", (Client) null);
		testSubject.setPassword(session.getSid(), 1L, "test");

		assertTrue(testSubject.unprotect(session.getSid(), 1L, "test"));
		assertFalse(testSubject.unprotect(session.getSid(), 1L, "test2222"));
		assertTrue(testSubject.unprotect(session.getSid(), 1L, "test"));
	}
}