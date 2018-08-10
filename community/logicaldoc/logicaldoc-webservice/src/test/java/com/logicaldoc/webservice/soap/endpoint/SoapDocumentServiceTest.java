package com.logicaldoc.webservice.soap.endpoint;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;

import junit.framework.Assert;

import org.junit.Test;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.webservice.AbstractWebServiceTestCase;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSNote;
import com.logicaldoc.webservice.model.WSRating;
import com.logicaldoc.webservice.model.WSUtil;

/**
 * Test case for <code>SoapDocumentService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapDocumentServiceTest extends AbstractWebServiceTestCase {

	private DocumentDAO docDao;

	private FolderDAO folderDao;

	// Instance under test
	private SoapDocumentService docService;

	@Override
	public void setUp() throws Exception {
		super.setUp();
		docDao = (DocumentDAO) context.getBean("DocumentDAO");
		folderDao = (FolderDAO) context.getBean("FolderDAO");

		// Make sure that this is a DocumentServiceImpl instance
		docService = new SoapDocumentService();
		docService.setValidateSession(false);
	}

	@Test
	public void testUpdate() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals(103, doc.getFolder().getId());
		Document newDoc = docDao.findById(2);
		Assert.assertNotNull(newDoc);
		Assert.assertEquals(103, newDoc.getFolder().getId());
		docDao.initialize(doc);
		docDao.initialize(newDoc);

		WSDocument wsDoc = WSUtil.toWSDocument(newDoc);
		Assert.assertEquals(2, wsDoc.getId());
		wsDoc.setId(1);
		wsDoc.setCustomId("xxxxxxxx");
		Assert.assertEquals(1, wsDoc.getId());
		Assert.assertEquals("pluto", wsDoc.getFileName());

		docService.update("", wsDoc);

		docDao.initialize(doc);
		Assert.assertEquals("pluto(1)", doc.getFileName());
	}

	@Test
	public void testAddNote() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);

		WSNote note = docService.addNote("", doc.getId(), "note1");
		note = docService.addNote("", doc.getId(), "note2");

		WSNote[] notes = docService.getNotes("", doc.getId());
		Assert.assertEquals(4, notes.length);
		docService.deleteNote("", note.getId());
		notes = docService.getNotes("", doc.getId());
		Assert.assertEquals(3, notes.length);
	}

	@Test
	public void testRateDocument() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);

		WSRating rating = docService.rateDocument("", doc.getId(), 3);
		Assert.assertNotNull(rating);
		Assert.assertEquals(3, rating.getVote());

		WSRating[] ratings = docService.getRatings("", doc.getId());
		Assert.assertEquals(1, ratings.length);
	}

	@Test
	public void testMove() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Folder folder = doc.getFolder();
		Assert.assertEquals(103, folder.getId());

		Folder newFolder = folderDao.findById(100);
		docDao.initialize(doc);
		doc.setIndexed(0);
		docDao.store(doc);
		docService.move("", doc.getId(), newFolder.getId());
		// NOTA: attenzione errore optimistic lock
		Assert.assertSame(1L, doc.getId());
		docDao.initialize(doc);
		Assert.assertEquals(newFolder, doc.getFolder());
	}

	@Test
	public void testLock() throws Exception {
		docService.unlock("", 1);

		docService.lock("", 1);

		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		docDao.initialize(doc);
		Assert.assertEquals(2, doc.getStatus());
		Assert.assertEquals(1L, doc.getLockUserId().longValue());
	}

	@Test
	public void testCreate() throws Exception {
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
		docService.create("xxxx", wsDoc, new DataHandler(new FileDataSource(file)));

		Document doc = docDao.findByFileNameAndParentFolderId(wsDoc.getFolderId(), wsDoc.getFileName(), null, 1L, null)
				.get(0);
		Assert.assertNotNull(doc);
		docDao.initialize(doc);

		Assert.assertEquals("document test.txt", doc.getFileName());
		Assert.assertEquals("coverage-val", doc.getValue("coverage"));

		wsDoc = docService.getDocument("xxxx", doc.getId());
		Assert.assertEquals("document test.txt", wsDoc.getFileName());
		Assert.assertEquals("coverage-val", wsDoc.getAttribute("coverage").getStringValue());
	}

	@Test
	public void testUpload() throws Exception {
		File file = new File("pom.xml");
		long docId = docService.upload("xxxx", null, 4L, true, "document test.txt", "en", new DataHandler(
				new FileDataSource(file)));

		Assert.assertTrue(docId > 0L);

		long docId2 = docService.upload("xxxx", docId, null, true, "document test.txt", "en", new DataHandler(
				new FileDataSource(file)));

		Assert.assertEquals(docId, docId2);
		Assert.assertEquals("2.0", docDao.findById(docId2).getVersion());
	}

	@Test
	public void testCheckin() throws Exception {
		docService.checkout("", 1);

		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		docDao.initialize(doc);
		Assert.assertEquals(Document.DOC_CHECKED_OUT, doc.getStatus());

		File file = new File("pom.xml");
		docService.checkin("", 1, "comment", "pom.xml", true, new DataHandler(new FileDataSource(file)));

		doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		docDao.initialize(doc);

		// Assert.assertEquals(AbstractDocument.INDEX_TO_INDEX,
		// doc.getIndexed());
		Assert.assertEquals(0, doc.getSigned());
		Assert.assertEquals(Document.DOC_UNLOCKED, doc.getStatus());
	}

	@Test
	public void testDelete() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		docService.delete("", doc.getId());
		doc = docDao.findById(1);
		Assert.assertNull(doc);
	}

	@Test
	public void testRename() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());
		docDao.initialize(doc);
		docService.rename("", 1, "pippo");
		docDao.initialize(doc);
		Assert.assertEquals("pippo", doc.getFileName());
	}

	@Test
	public void testRenameFile() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);
		Assert.assertEquals("pippo", doc.getFileName());
		docDao.initialize(doc);
		docService.renameFile("", 1, "pippo.doc");
		docDao.initialize(doc);
		Assert.assertEquals("pippo.doc", doc.getFileName());
		Assert.assertEquals("doc", doc.getType());
	}

	@Test
	public void testGetDocument() throws Exception {
		Document doc = docDao.findById(1);
		Assert.assertNotNull(doc);

		WSDocument wsDoc = docService.getDocument("", 1);

		Assert.assertEquals(1, wsDoc.getId());
		Assert.assertEquals("pippo", wsDoc.getFileName());
		Assert.assertEquals(103, wsDoc.getFolderId().longValue());
	}

	@Test
	public void testGetDocumentByCustomId() throws Exception {
		Document doc = docDao.findByCustomId("a", Tenant.DEFAULT_ID);
		Assert.assertNotNull(doc);

		WSDocument wsDoc = docService.getDocument("", 1);

		Assert.assertEquals(1, wsDoc.getId());
		Assert.assertEquals("pippo", wsDoc.getFileName());
		Assert.assertEquals(103, wsDoc.getFolderId().longValue());
	}

	@Test
	public void testIsReadable() throws Exception {
		Assert.assertTrue(docService.isReadable("", 1));
		Assert.assertFalse(docService.isReadable("", 99));
	}

	@Test
	public void testRestore() throws Exception {
		Assert.assertNull(docDao.findById(4));
		docService.restore("", 4, 5);
		Assert.assertNotNull(docDao.findById(4));
	}

	@Test
	public void testGetVersions() throws Exception {
		WSDocument[] versions = docService.getVersions("", 1);
		Assert.assertEquals(2, versions.length);
		List<WSDocument> versionsList = Arrays.asList(versions);
		Assert.assertEquals("testVer02", versionsList.get(0).getVersion());
		Assert.assertEquals("testVer01", versionsList.get(1).getVersion());

		versions = docService.getVersions("", 2);
		Assert.assertEquals(0, versions.length);
	}

	@Test
	public void testListDocuments() throws Exception {
		WSDocument[] docs = docService.listDocuments("", 103, null);
		Assert.assertNotNull(docs);
		Assert.assertEquals(2, docs.length);
		List<WSDocument> docsList = Arrays.asList(docs);
		Assert.assertEquals(1, docsList.get(0).getId());
		Assert.assertEquals(2, docsList.get(1).getId());

		docs = docService.listDocuments("", 103, "plo");
		Assert.assertNotNull(docs);
		Assert.assertEquals(0, docs.length);

		docs = docService.listDocuments("", 103, "*ut*");
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.length);
		Assert.assertEquals("pluto", docs[0].getFileName());

		docs = docService.listDocuments("", 103, "pippo");
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.length);
		Assert.assertEquals("pippo", docs[0].getFileName());
	}

	@Test
	public void testGetDocuments() throws Exception {
		WSDocument[] docs = docService.getDocuments("", new Long[] { 1L, 2L, 3L });
		Assert.assertNotNull(docs);
		Assert.assertEquals(2, docs.length);
	}

	@Test
	public void testGetAliases() throws Exception {
		WSDocument[] docs = docService.getAliases("", 1L);
		Assert.assertNotNull(docs);
		Assert.assertEquals(1, docs.length);

		docs = docService.getAliases("", 2L);
		Assert.assertNotNull(docs);
		Assert.assertEquals(0, docs.length);
	}

	@Test
	public void testSetPassword() throws Exception {
		Session session = SessionManager.get().newSession("admin", "admin", null);
		docService.setPassword(session.getSid(), 1L, "test");

		try {
			docService.unsetPassword(session.getSid(), 1L, "adsfddf");
		} catch (Throwable t) {
			Assert.assertNotNull(t);
		}

		docService.unsetPassword(session.getSid(), 1L, "test");
	}

	@Test
	public void testUnprotect() throws Exception {
		Session session = SessionManager.get().newSession("admin", "admin", null);
		docService.setPassword(session.getSid(), 1L, "test");

		Assert.assertTrue(docService.unprotect(session.getSid(), 1L, "test"));
		Assert.assertFalse(docService.unprotect(session.getSid(), 1L, "test2222"));
		Assert.assertTrue(docService.unprotect(session.getSid(), 1L, "test"));
	}
}