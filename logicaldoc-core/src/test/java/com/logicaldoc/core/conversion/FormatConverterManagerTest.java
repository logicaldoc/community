package com.logicaldoc.core.conversion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link FormatConverterManager}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class FormatConverterManagerTest extends AbstractCoreTestCase {

	private DocumentDAO docDao;

	private Store store;

	// Instance under test
	private FormatConverterManager testSubject;

	private Document document;

	@Override
	protected List<String> getPluginArchives() {
		return List.of("logicaldoc-core-plugin.jar");
	}

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		prepareSession("admin", "admin");

		docDao = Context.get(DocumentDAO.class);

		store = Context.get(Store.class);

		testSubject = Context.get(FormatConverterManager.class);

		DocumentManager documentManager = Context.get(DocumentManager.class);
		FolderDAO folderDao = Context.get(FolderDAO.class);

		document = new Document();
		document.setFileName("test.txt");
		document.setFolder(folderDao.findById(Folder.DEFAULTWORKSPACEID));

		try {
			DocumentHistory transaction = new DocumentHistory();
			transaction.setUserId(User.USERID_ADMIN);
			transaction.setUsername("admin");
			document = documentManager
					.create(ResourceUtil.getInputStream("data.sql"), document, transaction).get();
		} catch (PersistenceException | InterruptedException | ExecutionException e) {
			throw new IOException(e);
		}
	}

	@Test
	public void testConvert() throws PersistenceException, IOException {
		DocumentHistory transaction = new DocumentHistory();
		transaction.setUserId(User.USERID_ADMIN);
		transaction.setUsername("admin");

		try {
			testSubject.convert(document, document.getFileVersion(), "png", transaction);
			fail("there should be no converter");
		} catch (Exception e) {
			// all ok
		}

		Document converted = testSubject.convert(document, document.getFileVersion(), "pdf", transaction);
		assertNotNull(converted);
		assertEquals("pdf", FileUtil.getExtension(converted.getFileName()));
	}

	@Test
	public void testConvertFile() throws IOException {
		File file = FileUtil.createTempFile("test", ".pdf");

		try {
			testSubject.convertFile(new File("src/test/resources/data.sql"), "src.txt", file, "out.pdf",
					session.getSid());
			assertTrue(file.length() > 0);
		} finally {
			FileUtil.delete(file);
		}
	}

	@Test
	public void testConvertToPdf() throws IOException {
		long docId = document.getId();

		String pdfResource = store.getResourceName(docId, document.getFileVersion(),
				FormatConverterManager.PDF_CONVERSION_SUFFIX);
		store.delete(docId, pdfResource);
		assertFalse(store.exists(docId, pdfResource));

		testSubject.convertToPdf(document, session.getSid());
		assertTrue(store.exists(docId, pdfResource));

		assertTrue(testSubject.getPdfContent(document, document.getFileVersion(), session.getSid()).length > 0);
	}

	@Test
	public void testGetPdfContent() throws PersistenceException, IOException {
		// Convert first
		testSubject.convertToPdf(document, session.getSid());
		assertTrue(testSubject.getPdfContent(document, document.getFileVersion(), session.getSid()).length > 0);

		// Remove the conversion and invoke getPdfContent
		String pdfResource = store.getResourceName(document.getId(), document.getFileVersion(),
				FormatConverterManager.PDF_CONVERSION_SUFFIX);
		store.delete(document.getId(), pdfResource);
		assertTrue(testSubject.getPdfContent(document, document.getFileVersion(), session.getSid()).length > 0);

		// Use a PDF file
		Document doc = docDao.findById(1L);
		assertTrue(testSubject.getPdfContent(doc, doc.getFileVersion(), session.getSid()).length > 0);
	}

	@Test
	public void testWritePdfToFile() throws PersistenceException, IOException {
		File file = FileUtil.createTempFile("test", ".pdf");

		try {
			// Convert first
			testSubject.convertToPdf(document, session.getSid());
			testSubject.writePdfToFile(document, document.getFileVersion(), file, session.getSid());
			assertTrue(file.length() > 0);

			FileUtil.delete(file);
			assertEquals(0L, file.length());

			// Remove the conversion and invoke getPdfContent
			String pdfResource = store.getResourceName(document.getId(), document.getFileVersion(),
					FormatConverterManager.PDF_CONVERSION_SUFFIX);
			store.delete(document.getId(), pdfResource);
			testSubject.writePdfToFile(document, document.getFileVersion(), file, session.getSid());
			assertTrue(file.length() > 0);

			FileUtil.delete(file);
			assertEquals(0L, file.length());

			// Use a PDF file
			Document doc = docDao.findById(1L);
			assertTrue(testSubject.getPdfContent(doc, doc.getFileVersion(), session.getSid()).length > 0);
			testSubject.writePdfToFile(doc, doc.getFileVersion(), file, session.getSid());
			assertTrue(file.length() > 0);
		} finally {
			FileUtil.delete(file);
		}
	}

	@Test
	public void testGetAllOutputFormats() {
		assertEquals(1, testSubject.getAllOutputFormats("txt").size());
		assertEquals("pdf", testSubject.getAllOutputFormats("txt").get(0));

		assertEquals(0, testSubject.getEnabledOutputFormats("txt").size());

		assertEquals(28, testSubject.getAvailableInputFormats().size());
	}
}