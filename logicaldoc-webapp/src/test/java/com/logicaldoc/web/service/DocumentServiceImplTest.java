package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentLinkDAO;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIContact;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.AbstractWebappTestCase;
import com.logicaldoc.web.UploadServlet;

import jakarta.mail.MessagingException;

@RunWith(MockitoJUnitRunner.class)
public class DocumentServiceImplTest extends AbstractWebappTestCase {

	private static final String UTF_8 = "UTF-8";

	private static final Logger log = LoggerFactory.getLogger(DocumentServiceImplTest.class);

	@Mock
	private EMailSender emailSender;

	// Instance under test
	private DocumentServiceImpl testSubject = new DocumentServiceImpl();

	private DocumentDAO docDao;

	private FolderDAO folderDao;

	private Store store;

	private TemplateDAO templateDao;

	private DocumentLinkDAO linkDao;

	private DocumentNoteDAO noteDao;

	private BookmarkDAO bookDao;

	private DocumentHistoryDAO documentHistoryDao;

	protected SearchEngine searchEngine;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		docDao = Context.get(DocumentDAO.class);
		linkDao = Context.get(DocumentLinkDAO.class);
		noteDao = Context.get(DocumentNoteDAO.class);
		documentHistoryDao = Context.get(DocumentHistoryDAO.class);
		bookDao = Context.get(BookmarkDAO.class);
		templateDao = Context.get(TemplateDAO.class);
		folderDao = Context.get(FolderDAO.class);
		store = Context.get(Store.class);

		searchEngine = Context.get(SearchEngine.class);

		prepareRepository();

		emailSender = mock(EMailSender.class);
		try {
			doNothing().when(emailSender).send(any(EMail.class));
			DocumentServiceImpl.setEmailSender(emailSender);
		} catch (MessagingException e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	@Override
	public void tearDown() throws SQLException, IOException {
		searchEngine.unlock();
		searchEngine.close();

		super.tearDown();
	}

	private void prepareRepository() throws IOException {
		File file3 = new File(repositoryDir.getPath() + "/docs/3/doc/1.0");
		file3.getParentFile().mkdirs();
		FileUtil.copyResource("test.zip", file3);

		File file5 = new File(repositoryDir.getPath() + "/docs/5/doc/1.0");
		file5.getParentFile().mkdirs();
		FileUtil.copyResource("Joyce Jinks shared the Bruce Duo post.eml", file5);
		FileUtil.copyResource("Joyce Jinks shared the Bruce Duo post.eml",
				new File(repositoryDir.getPath() + "/docs/5/doc/1.0-conversion.pdf"));

		File file6 = new File(repositoryDir.getPath() + "/docs/6/doc/1.0");
		file6.getParentFile().mkdirs();
		FileUtil.copyResource("Hurry up! Only a few hours for the Prime Day VGA promos !!!.msg", file6);

		File file7 = new File(repositoryDir.getPath() + "/docs/7/doc/1.0");
		file7.getParentFile().mkdirs();
		FileUtil.copyResource("New error indexing documents.eml", file7);

		Map<String, File> uploadedFiles = new HashMap<>();
		uploadedFiles.put("file3.zip", file3);
		uploadedFiles.put("file5.eml", file5);
		uploadedFiles.put("file6.msg", file6);
		uploadedFiles.put("file7.eml", file7);

		session.getDictionary().put(UploadServlet.UPLOADS, uploadedFiles);
	}

	@Test
	public void testDestroyDocuments() throws ServerException, PersistenceException {
		assertNotNull(docDao.findById(1L));
		testSubject.destroyDocuments(List.of(1L));
		assertEquals(0L, docDao.queryForLong("select count(ld_id) from ld_document where ld_id=1"));
	}

	@Test
	public void testApplyParentFolderSecurity() throws ServerException, PersistenceException {
		assertEquals(0L, docDao.queryForLong("select count(*) from ld_document_acl where ld_docid=1"));
		testSubject.applyParentFolderSecurity(1L);
		assertEquals(4L, docDao.queryForLong("select count(*) from ld_document_acl where ld_docid=1"));
	}

	@Test
	public void testDeleteFromTrash() throws ServerException, PersistenceException {
		testSubject.delete(List.of(7L));
		List<Long> docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=1",
				Long.class);
		assertEquals(1, docIds.size());

		testSubject.deleteFromTrash(docIds);
		docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=1", Long.class);
		assertEquals(0, docIds.size());
		docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=2", Long.class);
		assertEquals(1, docIds.size());
	}

	@Test
	public void testEmptyTrash() throws ServerException, PersistenceException {
		testSubject.delete(List.of(7L));
		List<Long> docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=1",
				Long.class);
		assertEquals(1, docIds.size());

		testSubject.emptyTrash();
		docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=1", Long.class);
		assertEquals(0, docIds.size());
		docIds = (List<Long>) docDao.queryForList("select ld_id from ld_document where ld_deleted=2", Long.class);
		assertEquals(1, docIds.size());
	}

	@Test
	public void testArchiveAndUnarchiveDocuments() throws ServerException, PersistenceException {
		GUIDocument doc = testSubject.getById(7);
		testSubject.archiveDocuments(List.of(doc.getId()), "archive comment");

		Document document = docDao.findById(7);
		assertEquals(DocumentStatus.ARCHIVED, document.getStatus());

		testSubject.unarchiveDocuments(List.of(doc.getId()));
		document = docDao.findById(7);
		assertEquals(DocumentStatus.UNLOCKED, document.getStatus());
	}

	@Test
	public void testArchiveFolder() throws ServerException, PersistenceException {

		// Move a document inside the tree to archive
		Document doc = docDao.findById(5);
		docDao.initialize(doc);
		doc.setFolder(folderDao.findById(1201));
		docDao.store(doc);

		assertEquals(1, testSubject.archiveFolder(1200L, "archive comment"));

		Document document = docDao.findById(5);
		assertEquals(DocumentStatus.ARCHIVED, document.getStatus());
	}

	@Test
	public void testCreateDownloadTicket() throws ServerException {
		List<String> ticket = testSubject.createDownloadTicket(5L, 0, null, null, null, null, null);
		// We do not have a HTTP request so expect that the first string is the
		// exact ticket ID
		assertEquals("http://server:port/download-ticket?ticketId=" + ticket.get(0), ticket.get(1));

		TicketDAO tDao = (TicketDAO) context.getBean(TicketDAO.class);
		Ticket t = tDao.findByTicketId(ticket.get(0));
		assertNotNull(t);
		assertEquals(5L, t.getDocId());
	}

	@Test
	public void testDeleteEnableDisableTicket() throws ServerException {
		List<String> ticket = testSubject.createDownloadTicket(5, 0, null, null, null, null, null);

		// We do not have a HTTP request so expect that the first string is the
		// exact ticket ID
		TicketDAO tDao = (TicketDAO) context.getBean(TicketDAO.class);
		Ticket t = tDao.findByTicketId(ticket.get(0));
		assertNotNull(t);
		assertEquals(5L, t.getDocId());
		assertEquals(1, t.getEnabled());

		testSubject.disableTicket(t.getId());
		t = tDao.findByTicketId(ticket.get(0));
		assertEquals(0, t.getEnabled());

		testSubject.enableTicket(t.getId());
		t = tDao.findByTicketId(ticket.get(0));
		assertEquals(1, t.getEnabled());

		testSubject.deleteTicket(t.getId());
		t = tDao.findByTicketId(ticket.get(0));
		assertNull(t);
	}

	@Test
	public void testRename() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		assertEquals("New error indexing documents.eml", doc.getFileName());

		testSubject.rename(doc.getId(), "newname.eml");
		doc = testSubject.getById(7);
		assertEquals("newname.eml", doc.getFileName());
	}

	@Test
	public void testSetAndUnsetPassword() throws ServerException, PersistenceException {
		testSubject.setPassword(5, "pippo");
		Document doc = docDao.findById(5);
		assertNotNull(doc.getPassword());

		// Try to unset with wrong password but admin user
		testSubject.unsetPassword(5, "paperino");
		doc = docDao.findById(5);
		assertNull(doc.getPassword());
	}

	@Test
	public void testUprotect() throws ServerException, PersistenceException {
		testSubject.setPassword(5, "pippo");
		Document doc = docDao.findById(5);
		assertNotNull(doc.getPassword());

		// Try to uprotect with wrong password
		testSubject.unprotect(5, "paperino");
		assertTrue(session.getUnprotectedDocs().isEmpty());

		// Try to uprotect with correct password
		testSubject.unprotect(5, "pippo");
		assertTrue(session.getUnprotectedDocs().containsKey(5L));
	}

	@Test
	public void testCreateWithContent() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		doc.setId(0);
		doc.setFileName("testcontent.txt");
		doc.setCustomId(null);
		doc = testSubject.createWithContent(doc, "text content", true);
		assertNotNull(doc);
		assertNotSame(0L, doc.getId());

		doc = testSubject.getById(doc.getId());
		assertNotNull(doc);
		assertEquals("text content", store.getString(doc.getId(), store.getResourceName(doc.getId(), null, null)));
		testSubject.checkout(List.of(doc.getId()));

		doc.setId(0);
		doc.setFileName("testcontent2.txt");
		doc.setCustomId(null);
		doc = testSubject.createWithContent(doc, " ", true);
		assertNotNull(doc);
		assertNotSame(0L, doc.getId());

		doc = testSubject.getById(doc.getId());
		assertNotNull(doc);
		assertEquals(" ", store.getString(doc.getId(), store.getResourceName(doc.getId(), null, null)));
		testSubject.checkout(List.of(doc.getId()));
	}

	@Test
	public void testCheckinContent() throws ServerException, PersistenceException {
		testCreateWithContent();
		testSubject.checkout(List.of(7L));

		testSubject.checkinContent(7, "checkedin contents");

		// try to work on an alias
		Document alias = new Document();
		alias.setFileName("alias.txt");
		alias.setDocRef(7L);
		alias.setFolder(folderDao.findById(5L));

		assertEquals("checkedin contents", testSubject.getContentAsString(7L));

		docDao.store(alias);
		testSubject.checkout(List.of(alias.getId()));
		testSubject.checkinContent(alias.getId(), "checkedin contents2");

		assertEquals("checkedin contents2", testSubject.getContentAsString(alias.getId()));
	}

	@Test
	public void testSaveAndGetRating() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		assertEquals(0, doc.getRating());

		GUIRating rating = new GUIRating();
		rating.setDocId(doc.getId());
		rating.setUserId(User.USERID_ADMIN);
		rating.setUsername("admin");
		rating.setVote(4);

		testSubject.saveRating(rating);
		doc = testSubject.getById(doc.getId());
		assertEquals(4, doc.getRating());

		rating = testSubject.getRating(doc.getId());
		assertEquals("4.0", rating.getAverage().toString());
		assertEquals(Integer.valueOf(1), rating.getCount());

		rating = testSubject.getUserRating(doc.getId());
		assertEquals(4, rating.getVote());

		assertEquals(Integer.valueOf(0), testSubject.deleteRating(rating.getId()));
		rating = testSubject.getRating(doc.getId());
		assertEquals(0, rating.getVote());
	}

	@Test
	public void testReplaceFile() throws ServerException, IOException {
		GUIDocument doc = testSubject.getById(7);
		assertFalse(testSubject.getContentAsString(7).contains("replaced contents"));

		testSubject.cleanUploadedFileFolder();

		File tmpFile = new File("target/replacefile.txt");
		try {
			FileUtil.writeFile("replaced contents", tmpFile.getAbsolutePath());
			Map<String, File> uploadedFiles = new HashMap<>();
			uploadedFiles.put(doc.getFileName(), tmpFile);
			assertTrue(FileUtil.readFile(tmpFile).contains("replaced contents"));

			session.getDictionary().put(UploadServlet.UPLOADS, uploadedFiles);
			testSubject.replaceFile(doc.getId(), doc.getFileVersion(), "replace");

			assertTrue(testSubject.getContentAsString(7).contains("replaced contents"));
		} finally {
			FileUtil.delete(tmpFile);
		}
	}

	@Test
	public void testCreateDocument() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		doc.setId(0);
		doc.setFileName("test.txt");
		doc.setCustomId(null);

		doc = testSubject.createDocument(doc, "document content");
		assertNotNull(doc);

		assertEquals("document content", testSubject.getContentAsString(doc.getId()));
	}

	@Test
	public void testEnforceFilesIntoFolderStore() throws ServerException, PersistenceException {
		Folder folder = folderDao.findById(1200);
		folderDao.initialize(folder);
		assertEquals(Integer.valueOf(2), folder.getStore());

		Document doc = docDao.findById(5);
		docDao.initialize(doc);
		doc.setFolder(folderDao.findById(1201));
		docDao.store(doc);

		doc = docDao.findById(5);
		assertNull(doc.getFolder().getStore());

		testSubject.enforceFilesIntoFolderStore(1200);

		File movedFile = new File(repositoryDir + "/docs2/5/doc/1.0");
		assertTrue(movedFile.exists());
	}

	@Test
	public void testMakeImmutable() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		assertEquals(0, doc.getImmutable());

		testSubject.makeImmutable(List.of(7L), "immutable comment");

		doc = testSubject.getById(7);
		assertEquals(1, doc.getImmutable());
	}

	@Test
	public void testPromoteVersion() throws ServerException {
		testCheckin();
		GUIDocument doc = testSubject.getById(7);
		assertEquals(DocumentStatus.UNLOCKED.ordinal(), doc.getStatus());
		assertEquals("1.1", doc.getVersion());
		assertEquals("1.1", doc.getFileVersion());

		testSubject.promoteVersion(doc.getId(), "1.0");

		doc = testSubject.getById(7);
		assertEquals(DocumentStatus.UNLOCKED.ordinal(), doc.getStatus());
		assertEquals("1.2", doc.getVersion());
		assertEquals("1.2", doc.getFileVersion());

		// non-existent version
		boolean exceptionHappened = false;
		try {
			testSubject.promoteVersion(doc.getId(), "xxxx");
		} catch (ServerException e) {
			exceptionHappened = true;
			assertEquals("Unexisting version xxxx of document 7", e.getMessage());
		}
		assertTrue(exceptionHappened);

		// Document locked
		testSubject.lock(List.of(7L), "lock comment");
		exceptionHappened = false;
		try {
			testSubject.promoteVersion(doc.getId(), "1.0");
		} catch (ServerException e) {
			exceptionHappened = true;
			assertEquals("The document 7 is locked", e.getMessage());
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testCheckout() throws ServerException {
		GUIDocument doc = testSubject.getById(7);
		assertEquals(DocumentStatus.UNLOCKED.ordinal(), doc.getStatus());

		testSubject.checkout(List.of(7L));
		doc = testSubject.getById(7);
		assertEquals(DocumentStatus.CHECKEDOUT.ordinal(), doc.getStatus());
	}

	@Test
	public void testCheckin() throws ServerException {
		testCheckout();

		// Prepare the file to checkin
		Map<String, File> uploadedFiles = new HashMap<>();
		File file3 = new File(repositoryDir, "docs/3/doc/1.0");
		uploadedFiles.put("test.zip", file3);
		session.getDictionary().put(UploadServlet.UPLOADS, uploadedFiles);

		GUIDocument doc = testSubject.getById(7);
		assertEquals("1.0", doc.getVersion());
		assertEquals("1.0", doc.getFileVersion());

		doc.setComment("version comment");
		testSubject.checkin(doc, false);

		doc = testSubject.getById(7);
		assertEquals(DocumentStatus.UNLOCKED.ordinal(), doc.getStatus());
		assertEquals("1.1", doc.getVersion());
		assertEquals("1.1", doc.getFileVersion());
	}

	@Test
	public void testAddDocuments() throws ServerException, IOException, PersistenceException {
		GUIDocument doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);
		doc.setNotifyUsers(List.of(2L, 3L));

		List<GUIDocument> createdDocs = testSubject.addDocuments(false, UTF_8, false, doc);
		assertEquals(4, createdDocs.size());

		testSubject.cleanUploadedFileFolder();
		prepareRepository();
		doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);

		// Request immediate indexing
		createdDocs = testSubject.addDocuments(false, UTF_8, true, doc);
		assertEquals(4, createdDocs.size());

		prepareRepository();
		doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);

		// Request zip import so just the other 3 documents are imported
		// immediately
		createdDocs = testSubject.addDocuments(true, UTF_8, false, doc);
		assertEquals(3, createdDocs.size());

		// Remove the uploaded files
		@SuppressWarnings("unchecked")
		Map<String, File> uploadedFiles = (Map<String, File>) session.getDictionary().get(UploadServlet.UPLOADS);
		uploadedFiles.clear();

		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);
		boolean exceptionHappened = false;
		try {
			testSubject.addDocuments(false, UTF_8, false, doc);
		} catch (ServerException e) {
			exceptionHappened = true;
			assertEquals("No file uploaded", e.getMessage());
		}
		assertTrue(exceptionHappened);

		// Try with a user without permissions
		prepareRepository();

		doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);
		doc.setFolder(new FolderServiceImpl().getFolder(1201, false, false, false));
		prepareSession("boss", "admin");
		prepareRepository();

		createdDocs = testSubject.addDocuments(true, UTF_8, false, doc);
		assertEquals(0, createdDocs.size());

		prepareSession("admin", "admin");
		prepareRepository();
		createdDocs = testSubject.addDocuments("en", 1201, false, UTF_8, false, null);
		assertEquals(4, createdDocs.size());

		// Cannot add documents into the root
		exceptionHappened = false;
		try {
			testSubject.addDocuments("en", Folder.ROOTID, false, UTF_8, false, null);
		} catch (ServerException e) {
			exceptionHappened = true;
			assertEquals("Cannot add documents in the root", e.getMessage());
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testMerge() throws ServerException, IOException {
		GUIDocument doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);

		File pdf1 = new File(tempDir, "pdf1.pdf");
		File pdf2 = new File(tempDir, "pdf2.pdf");

		try {
			FileUtil.copyResource("pdf1.pdf", pdf1);
			FileUtil.copyResource("pdf2.pdf", pdf2);

			Map<String, File> uploadedFiles = new HashMap<>();
			uploadedFiles.put(pdf1.getName(), pdf1);
			uploadedFiles.put(pdf2.getName(), pdf2);

			session.getDictionary().put(UploadServlet.UPLOADS, uploadedFiles);

			List<GUIDocument> createdDocs = testSubject.addDocuments(false, UTF_8, false, doc);
			assertEquals(2, createdDocs.size());

			GUIDocument mergedDoc = testSubject.merge(createdDocs.stream().map(d -> d.getId()).toList(), 1200,
					"merged.pdf");
			mergedDoc = testSubject.getById(mergedDoc.getId());
			assertNotNull(mergedDoc);
			assertEquals("merged.pdf", mergedDoc.getFileName());
		} finally {
			FileUtil.delete(pdf1);
			FileUtil.delete(pdf2);
		}
	}

	@Test
	public void testUpdatePages() throws ServerException, IOException {
		GUIDocument doc = testSubject.getById(7);
		doc.setId(0L);
		doc.setCustomId(null);
		doc.setIndexed(0);

		File pdf2 = new File(tempDir, "pdf2.pdf");
		try {
			FileUtil.copyResource("pdf2.pdf", pdf2);

			Map<String, File> uploadedFiles = new HashMap<>();
			uploadedFiles.put(pdf2.getName(), pdf2);

			session.getDictionary().put(UploadServlet.UPLOADS, uploadedFiles);

			List<GUIDocument> createdDocs = testSubject.addDocuments(false, UTF_8, false, doc);
			assertEquals(1, createdDocs.size());
			assertEquals(2, createdDocs.get(0).getPages());

			assertEquals(2, testSubject.updatePages(createdDocs.get(0).getId()));
		} finally {
			FileUtil.delete(pdf2);
		}
	}

	@Test
	public void testReplaceAlias() throws ServerException, PersistenceException {
		DocumentManager manager = (DocumentManager) context.getBean("documentManager");
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUser(session.getUser());
		transaction.setSession(session);

		Document alias = manager.createAlias(docDao.findById(5), folderDao.findById(1201), null, transaction);
		assertEquals(Long.valueOf(5), alias.getDocRef());
		GUIDocument newFile = testSubject.replaceAlias(alias.getId());

		assertNull(docDao.findById(alias.getId()));
		Document newDoc = docDao.findById(newFile.getId());
		assertNotNull(newDoc);
		assertNull(newDoc.getDocRef());
		assertEquals(alias.getFileName(), newDoc.getFileName());
	}

	@Test
	public void testDeduplicate() throws ServerException, PersistenceException {
		Document doc5 = docDao.findById(5);
		docDao.initialize(doc5);
		doc5.setDigest("pippo");
		docDao.store(doc5);

		Document newDoc = new Document(doc5);
		newDoc.setId(0);
		newDoc.setCustomId(null);
		newDoc.setCreation(new Date());
		newDoc.setDate(new Date());
		newDoc.setFolder(folderDao.findById(1201));
		docDao.store(newDoc);

		testSubject.deDuplicate(null, true);

		assertNull(testSubject.getById(doc5.getId()));
		GUIDocument dc = testSubject.getById(newDoc.getId());
		assertNotNull(dc);
		assertEquals(1201, dc.getFolder().getId());
	}

	@Test
	public void testConvert() throws ServerException, IOException, PluginException {
		initializePlugins(List.of("/logicaldoc-core-plugin.jar"));

		GUIDocument doc = testSubject.getById(7);
		GUIDocument conversion = testSubject.convert(doc.getId(), doc.getFileVersion(), "pdf");
		conversion = testSubject.getById(conversion.getId());
		assertNotNull(conversion);
		assertTrue(conversion.getFileName().endsWith(".pdf"));
	}

	@Test
	public void testIndex() throws ServerException, IOException {
		searchEngine.init();

		GUIDocument doc = testSubject.getById(7);
		doc.setIndexed(0);
		doc.setFileName("test.txt");
		testSubject.save(doc);

		doc = testSubject.getById(7);
		assertEquals(0, doc.getIndexed());
		assertEquals("test.txt", doc.getFileName());

		FileUtil.copyResource("New error indexing documents.eml",
				new File(repositoryDir.getPath() + "/docs/" + doc.getId() + "/doc/" + doc.getFileVersion()));

		testSubject.indexDocuments(List.of(doc.getId()));
		doc = testSubject.getById(doc.getId());

		GUIDocument indexedDoc = testSubject.getById(doc.getId());
		assertEquals(1, indexedDoc.getIndexed());

		testSubject.indexDocuments(new ArrayList<>());
	}

	@Test
	public void testGetContentAsString() throws ServerException, IOException {
		GUIDocument doc = testSubject.getById(7);
		doc.setIndexed(0);
		doc.setFileName("test.txt");
		testSubject.save(doc);

		doc = testSubject.getById(7);
		assertEquals(0, doc.getIndexed());
		assertEquals("test.txt", doc.getFileName());

		FileUtil.copyResource("New error indexing documents.eml",
				new File(repositoryDir.getPath() + "/docs/" + doc.getId() + "/doc/" + doc.getFileVersion()));
		assertTrue(testSubject.getContentAsString(doc.getId()).contains("Gracias por tu pronta respuesta"));
	}

	@Test
	public void testGetVersionsById() throws ServerException {
		List<GUIVersion> versions = testSubject.getVersionsById(1, 2);
		assertNotNull(versions);
		assertEquals(2, versions.size());

		// only the first version of the two
		versions = testSubject.getVersionsById(1, 23);
		assertNotNull(versions);
		assertEquals(1, versions.size());

		// only the 2nd version of the two
		versions = testSubject.getVersionsById(21, 2);
		assertNotNull(versions);
		assertEquals(1, versions.size());

		// no versions
		versions = testSubject.getVersionsById(21, 22);
		assertNotNull(versions);
		assertEquals(0, versions.size());
	}

	@Test
	public void testDeleteVersions() throws ServerException {
		List<Long> ids = List.of(21L, 22L, 23L);
		GUIDocument gdoc;
		try {
			testSubject.deleteVersions(ids);
			fail("No exception here");
		} catch (ServerException e) {
			// All ok
		}

		ids = List.of(1L, 2L);
		gdoc = testSubject.deleteVersions(ids);
		assertNotNull(gdoc);
		assertEquals(1, gdoc.getId());
	}

	@Test
	public void testGetById() throws ServerException {
		GUIDocument doc = testSubject.getById(1);
		assertEquals(1, doc.getId());
		assertEquals("pippo", doc.getFileName());
		assertNotNull(doc.getFolder());
		assertEquals(5, doc.getFolder().getId());
		assertEquals("/", doc.getFolder().getName());

		doc = testSubject.getById(3);
		assertEquals(3, doc.getId());
		assertEquals("test.zip", doc.getFileName());

		// Try with unexisting document
		doc = testSubject.getById(99);
		assertNull(doc);
	}

	@Test
	public void testSave() throws Exception {
		GUIDocument doc = testSubject.getById(1);

		doc = testSubject.save(doc);
		assertNotNull(doc);
		assertEquals("myself", doc.getPublisher());

		doc = testSubject.getById(3);
		assertEquals("test.zip", doc.getFileName());

		doc = testSubject.save(doc);
		assertNotNull(doc);
	}

	@Test
	public void testUpdateLink() throws ServerException, PersistenceException {
		DocumentLink link = linkDao.findById(1);
		assertNotNull(link);
		assertEquals("test", link.getType());

		testSubject.updateLink(1, "pippo");

		link = linkDao.findById(1);
		assertNotNull(link);
		assertEquals("pippo", link.getType());
	}

	@Test
	public void testDeleteLinks() throws ServerException, PersistenceException {
		DocumentLink link = linkDao.findById(1);
		assertNotNull(link);
		assertEquals("test", link.getType());
		link = linkDao.findById(2);
		assertNotNull(link);
		assertEquals("xyz", link.getType());

		testSubject.deleteLinks(List.of(1L, 2L));

		link = linkDao.findById(1);
		assertNull(link);
		link = linkDao.findById(2);
		assertNull(link);
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals("pippo", doc.getFileName());
		doc = docDao.findById(2);
		assertNotNull(doc);
		assertEquals("pippo", doc.getFileName());
		assertEquals(1, doc.getDocRef().longValue());
		doc = docDao.findById(3);
		assertNotNull(doc);
		assertEquals("test.zip", doc.getFileName());

		doc = docDao.findById(1);
		assertNotNull(doc);
		testSubject.delete(List.of(2L, 3L));

		doc = docDao.findById(1);
		assertNotNull(doc);
		doc = docDao.findById(2);
		assertNull(doc);
		doc = docDao.findById(3);
		assertNull(doc);
	}

	@Test
	public void testDeleteNotes() throws ServerException, PersistenceException {
		List<DocumentNote> notes = noteDao.findByDocId(1L, "1.0");
		assertNotNull(notes);
		assertEquals(2, notes.size());
		assertEquals("message for note 1", notes.get(0).getMessage());

		testSubject.deleteNotes(List.of(notes.get(0).getId()));

		notes = noteDao.findByDocId(1L, "1.0");
		assertNotNull(notes);
		assertEquals(1, notes.size());
	}

	@Test
	public void testAddNote() throws ServerException, PersistenceException {
		List<DocumentNote> notes = noteDao.findByDocId(1L, "1.0");
		assertNotNull(notes);
		assertEquals(2, notes.size());

		long noteId = testSubject.addNote(1L, "pippo");

		DocumentNote note = noteDao.findById(noteId);
		assertNotNull(note);
		assertEquals("pippo", note.getMessage());

		notes = noteDao.findByDocId(1L, "1.0");
		assertNotNull(notes);
		assertEquals(2, notes.size());

		boolean exceptionHappened = false;
		try {
			// add note to a non existent doc
			testSubject.addNote(21L, "Midnight Rain");
		} catch (ServerException e) {
			exceptionHappened = true;
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testLock() throws ServerException, PersistenceException {
		Document doc = docDao.findById(1);
		assertNotNull(doc);
		assertEquals(3L, doc.getLockUserId().longValue());
		doc = docDao.findById(2);
		assertNotNull(doc);
		assertEquals(3L, doc.getLockUserId().longValue());

		testSubject.unlock(List.of(1L, 2L));

		doc = docDao.findDocument(1);
		assertNotNull(doc);
		assertNull(doc.getLockUserId());
		doc = docDao.findDocument(2);
		assertNotNull(doc);
		assertNull(doc.getLockUserId());

		testSubject.lock(List.of(1L, 2L), "comment");

		doc = docDao.findDocument(1);
		assertEquals(1L, doc.getLockUserId().longValue());
		doc = docDao.findDocument(2);
		assertEquals(1L, doc.getLockUserId().longValue());
	}

	@Test
	public void testLinkDocuments() throws ServerException, PersistenceException {
		testSubject.linkDocuments(List.of(1L, 2L), List.of(3L, 4L));

		DocumentLink link = linkDao.findByDocIdsAndType(1, 3, "default");
		assertNotNull(link);
		link = linkDao.findByDocIdsAndType(1, 4, "default");
		assertNotNull(link);
		link = linkDao.findByDocIdsAndType(2, 3, "default");
		assertNotNull(link);
		link = linkDao.findByDocIdsAndType(2, 4, "default");
		assertNotNull(link);
		link = linkDao.findByDocIdsAndType(3, 4, "default");
		assertNull(link);
	}

	@Test
	public void testRestore() throws ServerException, PersistenceException {
		docDao.delete(4);
		assertNull(docDao.findById(4));
		testSubject.restore(List.of(4L), 5);
		assertNotNull(docDao.findById(4));
		assertNotNull(docDao.findById(4));
		assertEquals(5L, docDao.findById(4).getFolder().getId());
	}

	@Test
	public void testBookmarks() throws ServerException, PersistenceException {
		testSubject.addBookmarks(List.of(1L, 2L), 0);

		Bookmark book = bookDao.findByUserIdAndDocId(1, 1);

		assertNotNull(book);
		book = bookDao.findByUserIdAndDocId(1, 2);
		assertNotNull(book);

		GUIBookmark bookmark = new GUIBookmark();
		bookmark.setId(book.getId());
		bookmark.setName("bookmarkTest");
		bookmark.setDescription("bookDescr");

		testSubject.updateBookmark(bookmark);
		book = bookDao.findById(bookmark.getId());
		assertNotNull(book);
		assertEquals("bookmarkTest", book.getTitle());
		assertEquals("bookDescr", book.getDescription());

		testSubject.deleteBookmarks(List.of(bookmark.getId()));

		book = bookDao.findById(1);
		assertNull(book);
		book = bookDao.findById(2);
		assertNull(book);

		// delete an already deleted bookmark
		testSubject.deleteBookmarks(List.of(bookmark.getId()));

		// Add bookmarks on folders
		testSubject.addBookmarks(List.of(6L, 7L), Bookmark.TYPE_FOLDER);

		// Add bookmarks on non existent documents
		boolean exceptionHappened = false;
		try {
			testSubject.addBookmarks(List.of(21L, 22L), Bookmark.TYPE_DOCUMENT);
		} catch (ServerException e) {
			exceptionHappened = true;
		}
		assertTrue(exceptionHappened);
	}

	@Test
	public void testMarkHistoryAsRead() throws ServerException, PersistenceException {
		List<DocumentHistory> histories = documentHistoryDao.findByUserIdAndEvent(1, "data test 01", null);
		assertEquals(2, histories.size());
		assertEquals(1, histories.get(0).getIsNew());
		assertEquals(1, histories.get(1).getIsNew());

		testSubject.markHistoryAsRead("data test 01");

		histories = documentHistoryDao.findByUserIdAndEvent(1, "data test 01", null);
		assertEquals(2, histories.size());
		assertEquals(0, histories.get(0).getIsNew());
		assertEquals(0, histories.get(1).getIsNew());
	}

	@Test
	public void testIndexable() throws ServerException, PersistenceException {
		Document doc1 = docDao.findById(1);
		assertNotNull(doc1);
		assertEquals(IndexingStatus.INDEXED, doc1.getIndexed());
		Document doc2 = docDao.findById(2);
		assertNotNull(doc2);
		assertEquals(IndexingStatus.TO_INDEX, doc2.getIndexed());
		Document doc3 = docDao.findById(3);
		assertNotNull(doc3);
		assertEquals(IndexingStatus.INDEXED, doc3.getIndexed());
		testSubject.markUnindexable(List.of(1L, 2L, 3L));

		doc1 = docDao.findById(1);
		assertNotNull(doc1);
		assertEquals(IndexingStatus.SKIP, doc1.getIndexed());
		doc2 = docDao.findById(2);
		assertNotNull(doc2);
		assertEquals(IndexingStatus.SKIP, doc2.getIndexed());
		doc3 = docDao.findById(3);
		assertNotNull(doc3);
		assertEquals(IndexingStatus.SKIP, doc3.getIndexed());

		testSubject.markIndexable(List.of(1L, 3L), IndexingStatus.TO_INDEX.ordinal());

		doc1 = docDao.findById(1);
		assertNotNull(doc1);
		assertEquals(IndexingStatus.TO_INDEX, doc1.getIndexed());
		doc3 = docDao.findById(3);
		assertNotNull(doc3);
		assertEquals(IndexingStatus.TO_INDEX, doc3.getIndexed());
	}

	@Test
	public void testCountDocuments() throws ServerException {
		assertEquals(7, testSubject.countDocuments(List.of(5L), 0));
		assertEquals(0, testSubject.countDocuments(List.of(5L), 3));
	}

	@Test
	public void testValidate() throws PersistenceException, ServerException {
		/*
		 * validate a simple document (no template assigned)
		 */
		GUIDocument gdoc = testSubject.getById(1);
		testSubject.validate(gdoc);

		// Update the document add a template
		Document doc = docDao.findDocument(6);
		docDao.initialize(doc);

		Template template = templateDao.findById(5L);
		templateDao.initialize(template);

		// Set the validator for attribute "attr1" to be email format
		template.getTemplateAttribute("attr1").setValidation(
				"#if(!$value.matches('^([\\w-\\.]+){1,64}@([\\w&&[^_]]+){2,255}.[a-z]{2,}$')) $error.setDescription($I18N.get('invalidformat')); #end");
		templateDao.store(template);

		doc.setTemplate(template);
		docDao.store(doc);

		gdoc = testSubject.getById(6);

		/*
		 * validate a document with template assigned
		 */

		// The value of attribute "attr1" is: "val1" so this should produce
		// an error
		try {
			testSubject.validate(gdoc);
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

		gdoc = testSubject.getById(6);

		// The value of attribute "attr1" is "test.xx@acme.de" this will
		// validate correctly
		testSubject.validate(gdoc);
	}

	@Test
	public void testSendAsEmail() throws Exception {
		// Send the email as download ticket
		GUIEmail guiMail = testSubject.extractEmail(5, "1.0");
		log.info(guiMail.getFrom().getEmail());
		guiMail.setDocIds(List.of(5L));

		List<GUIContact> tos = new ArrayList<>();
		GUIContact gc = new GUIContact("Kenneth", "Botterill", "ken-botterill@acme.com");
		tos.add(gc);

		guiMail.setTos(tos);

		tos = new ArrayList<>();
		gc = new GUIContact("Riley", "Arnold", "riley-arnold@acme.com");
		tos.add(gc);
		guiMail.setBccs(tos);

		tos = new ArrayList<>();
		gc = new GUIContact("Scout", "Marsh", "s.marsh@acme.com");
		tos.add(gc);
		guiMail.setCcs(tos);
		guiMail.setSendAsTicket(true);

		String retvalue = testSubject.sendAsEmail(guiMail, "en-US");
		log.info("returned message: {}", retvalue);
		assertEquals("ok", retvalue);

		// Send the email with attached .zip
		guiMail = testSubject.extractEmail(5, "1.0");
		log.info(guiMail.getFrom().getEmail());
		guiMail.setDocIds(List.of(5L));

		tos = new ArrayList<>();
		gc = new GUIContact("Kenneth", "Botterill", "ken-botterill@acme.com");
		tos.add(gc);

		guiMail.setTos(tos);
		guiMail.getBccs().clear();
		guiMail.getCcs().clear();

		guiMail.setSendAsTicket(false);
		guiMail.setZipCompression(true);

		retvalue = testSubject.sendAsEmail(guiMail, "en-US");
		log.info("returned message: {}", retvalue);
		assertEquals("ok", retvalue);

		// Send the email with attached file
		guiMail.setZipCompression(false);

		retvalue = testSubject.sendAsEmail(guiMail, "en-US");
		log.info("returned message: {}", retvalue);
		assertEquals("ok", retvalue);

		// Send the email with attached file as pdf conversion
		guiMail.setPdfConversion(true);
		retvalue = testSubject.sendAsEmail(guiMail, "en-US");
		log.info("returned message: {}", retvalue);
		assertEquals("ok", retvalue);
	}

	@Test
	public void testGetNotes() throws ServerException {
		// test on a non existent doc
		try {
			testSubject.getNotes(600, "1.0", null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// test on a doc without notes
		List<GUIDocumentNote> notes = testSubject.getNotes(6, "1.0", null);
		assertEquals(0, notes.size());

		// get a document with a single note
		notes = testSubject.getNotes(4, "1.0", null);
		assertEquals(1, notes.size());

		notes = testSubject.getNotes(4, null, null);
		assertEquals(1, notes.size());

	}

	@Test
	public void testUpdateNote() throws ServerException {
		List<GUIDocumentNote> notes = testSubject.getNotes(4, null, null);
		assertEquals(1, notes.size());
		assertEquals("message for note 3", notes.get(0).getMessage());

		testSubject.updateNote(4, notes.get(0).getId(), null, "updated message");
		List<GUIDocumentNote> notes2 = testSubject.getNotes(4, null, null);
		assertEquals(1, notes2.size());
		assertEquals(notes.get(0).getId(), notes2.get(0).getId());
		assertEquals("updated message", notes2.get(0).getMessage());
	}

	@Test
	public void testSaveNotes() throws ServerException {
		boolean exceptionHappened = false;
		try {
			List<GUIDocumentNote> notes = new ArrayList<>();
			testSubject.saveNotes(888, null, notes, null);
		} catch (ServerException e) {
			exceptionHappened = true;
		}
		assertTrue(exceptionHappened);

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
		testSubject.saveNotes(5, null, notes, null);
	}

	@Test
	public void testBulkUpdate() throws ParseException, PersistenceException, ServerException {
		GUIDocument doc4 = testSubject.getById(4L);
		assertNull(doc4.getAttribute("attr1"));

		List<Long> ids = List.of(4L);
		GUIDocument vo = new GUIDocument();
		vo.setPublished(1);

		String sDate1 = "10-21-2022";
		Date date1 = new SimpleDateFormat("MM-dd-yyyy").parse(sDate1);
		String sDate2 = "12-31-2089";
		Date date2 = new SimpleDateFormat("MM-dd-yyyy").parse(sDate2);

		vo.setStartPublishing(date1);
		vo.setStopPublishing(date2);
		vo.setLanguage("en");
		vo.setTags(List.of("Maroon", "Anti-Hero", "Karma"));
		vo.setTemplateId(5L);

		// set attributes
		List<GUIAttribute> attributes = new ArrayList<>();
		GUIAttribute gat = new GUIAttribute();
		gat.setName("attr1");
		gat.setType(0);
		gat.setStringValue("Snow on the Beach");
		attributes.add(gat);
		vo.setAttributes(attributes);

		try {
			List<GUIDocument> gdocs = testSubject.bulkUpdate(ids, vo, true);
			assertNotNull(gdocs);
			assertTrue(gdocs.size() > 0);
			assertNotNull(gdocs.get(0).getTags());
			assertEquals(3, gdocs.get(0).getTags().size());

			GUIAttribute gatX = gdocs.get(0).getAttribute("attr1");
			assertNotNull(gatX);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}

		// Test with a doc locked

		Document doc = docDao.findDocument(5);
		docDao.initialize(doc);
		doc.setStatus(DocumentStatus.CHECKEDOUT);
		docDao.store(doc);

		ids = List.of(5L, 6L);
		vo = new GUIDocument();
		vo.setPublished(0);
		vo.setColor("yellow");

		try {
			List<GUIDocument> gdocs = testSubject.bulkUpdate(ids, vo, true);
			assertNotNull(gdocs);
			assertFalse(gdocs.isEmpty());

			// only one document updated because 1 was locked (checked-out)
			assertEquals(1, gdocs.size());
			assertEquals("yellow", gdocs.get(0).getColor());
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testExtractEmail() {

		// test with document that is not an email (wrong or no extension)
		try {
			testSubject.extractEmail(4, null);
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// test with document that is a .msg email file
		try {
			testSubject.extractEmail(6, null);
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testSaveEmailAttachment() {

		// try to extract an attachment from a non email document
		try {
			testSubject.saveEmailAttachment(4, "1.0", "data.sql");
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// try to save an attachment that is not present in the document
		try {
			testSubject.saveEmailAttachment(5, "1.0", "data.sql");
			fail("Expected exception was not thrown");
		} catch (ServerException e) {
			// nothing to do
		}

		// try to save an attachment that is not present in the document
		try {
			testSubject.saveEmailAttachment(7, "1.0", "2022-01-04_15h54_11.png");
		} catch (ServerException e) {
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testGetEnabledPermissions() throws PersistenceException, ServerException {
		GUIAccessControlEntry permissions = testSubject.getAllowedPermissions(List.of(2L, 3L, 4L));
		for (Permission permission : Permission.all())
			assertTrue("Does not allow " + permission.name(),
					permissions.isPermissionAllowed(permission.name().toLowerCase()));

		prepareSession("boss", "admin");
		permissions = testSubject.getAllowedPermissions(List.of(2L, 3L, 4L));
		assertFalse(permissions.isRead());
		assertFalse(permissions.isPreview());

		prepareSession("author", "admin");
		permissions = testSubject.getAllowedPermissions(List.of(2L, 3L, 4L));
		assertTrue(permissions.isRead());
		assertTrue(permissions.isPreview());
		assertTrue(permissions.isReadingreq());
		assertTrue(permissions.isPrint());
		assertEquals(4, permissions.getAllowedPermissions().size());
	}
}