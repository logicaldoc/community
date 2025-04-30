package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

/**
 * Test case for {@link SoapSearchService}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class SoapSearchServiceTest extends AbstractWebserviceTestCase {

	private DocumentDAO documentDao;

	private SearchEngine searchEngine;

	private SoapSearchService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		documentDao = Context.get(DocumentDAO.class);
		searchEngine = Context.get(SearchEngine.class);

		testSubject = new SoapSearchService();
		testSubject.setValidateSession(false);

		try {
			addHits();
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("logicaldoc-core-plugin.jar");
	}

	@Test
	public void testFind() throws Exception {
		WSSearchOptions options = new WSSearchOptions();
		options.setExpression("paper");

		WSSearchResult result = testSubject.find("", options);
		assertNotNull(result);
		assertEquals(0, result.getTotalHits());
		
		options.setExpression("document");
		options.setLanguage("en");
		options.setExpressionLanguage("en");
		options.setFields(List.of("content"));
		result = testSubject.find("", options);
		assertNotNull(result);
		assertEquals(1, result.getTotalHits());
	}

	@Test
	public void testFindByFilename() throws Exception {
		List<WSDocument> documents = testSubject.findByFilename("", "pluto");
		assertNotNull(documents);
		assertEquals(1, documents.size());

		assertEquals(2, documents.get(0).getId());

		documents = testSubject.findByFilename("", "PLUTO");
		assertNotNull(documents);
		assertEquals(1, documents.size());

		assertEquals(2, documents.get(0).getId());

		documents = testSubject.findByFilename("", "paperino");
		assertNotNull(documents);
		assertEquals(0, documents.size());

		Document doc = documentDao.findById(1);
		assertNotNull(doc);
		assertEquals("pippo", doc.getFileName());
		documentDao.initialize(doc);
		doc.setFileName("paperina");
		documentDao.store(doc);
		assertEquals("paperina", doc.getFileName());

		documents = testSubject.findByFilename("", "pluto");
		assertNotNull(documents);
		assertEquals(1, documents.size());
	}

	@Test
	public void testFindFolders() throws Exception {
		List<WSFolder> folders = testSubject.findFolders("", "menu.admin");
		assertNotNull(folders);
		assertEquals(4, folders.size());
		assertEquals(80, folders.get(0).getId());
		assertEquals(99, folders.get(1).getId());

		folders = testSubject.findFolders("", "menu.adminxx");
		assertEquals(1, folders.size());
		assertEquals(80, folders.get(0).getId());

		folders = testSubject.findFolders("", "qqqxxx");
		assertNotNull(folders);
		assertEquals(0, folders.size());
	}

	private void addHits() throws Exception {
		Document document = new Document();
		document.setId(1L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold = new Folder();
		fold.setId(Folder.DEFAULTWORKSPACEID);
		fold.setName("test");
		document.setFolder(fold);
		documentDao.initialize(document);
		searchEngine.addHit(document, "Questo e un documento di prova. Per fortuna che esistono i test. document");

		// Adding unexisting document 111
		document = new Document();
		document.setId(111L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		searchEngine.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response.");

		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		searchEngine.addHit(document, "Another document");

		document = new Document();
		document.setId(3L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		searchEngine.addHit(document,
				"Lorem ipsum dolor sit amet, consectetur 5568299afbX0 ZKBKCHZZ80A CH8900761016116097873 adipisicing elit");
	}
}