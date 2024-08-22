package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.searchengine.folder.FolderCriterion;
import com.logicaldoc.core.searchengine.saved.SearchDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;

public class SearchServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private SearchServiceImpl testSubject = new SearchServiceImpl();

	protected SearchEngine engine;

	protected SearchDAO searchDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		engine = (SearchEngine) context.getBean("SearchEngine");
		searchDao = (SearchDAO) context.getBean("SearchDAO");
		try {
			addHits();
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	@Test
	public void testSearch() throws ServerException {
		GUISearchOptions options = new GUISearchOptions();
		options.setUserId(1L);
		options.setLanguage("en");
		options.setExpression("document");
		options.setExpressionLanguage("en_UK");
		options.setType(SearchOptions.TYPE_FULLTEXT);
		options.setDateFrom(new Date(1));
		options.setTopOperator("and");

		GUIResult result = testSubject.search(options);
		assertEquals(1, result.getHits().size());

		options.setExpression("unexisting");
		result = testSubject.search(options);
		assertTrue(result.getHits().isEmpty());

		options.setExpression(null);
		options.setType(SearchOptions.TYPE_FOLDERS);
		GUICriterion crit = new GUICriterion();
		crit.setField("name");
		crit.setStringValue("folder");
		crit.setOperator(FolderCriterion.OPERATOR_CONTAINS);
		options.getCriteria().add(crit);
		
		crit = new GUICriterion();
		crit.setField("id");
		crit.setLongValue(-1000L);
		crit.setOperator(FolderCriterion.OPERATOR_GREATER);
		
		crit = new GUICriterion();
		crit.setField("creation");
		crit.setDateValue(new Date(1));
		crit.setOperator(FolderCriterion.OPERATOR_GREATER);
		options.getCriteria().add(crit);
		
		result = testSubject.search(options);
		assertEquals(3, result.getHits().size());
	}

	@Test
	public void testCRUD() throws ServerException, PersistenceException {
		GUISearchOptions options = new GUISearchOptions();
		options.setUserId(1L);
		options.setLanguage("en");
		options.setExpression("document");
		options.setType(SearchOptions.TYPE_FULLTEXT);
		options.setName("test");

		testSubject.save(options);

		GUISearchOptions loaded = testSubject.load("test");
		assertNotNull(loaded);
		assertEquals("document", options.getExpression());
		assertEquals("test", options.getName());

		assertEquals(1, SearchServiceImpl.getSearches(session).size());

		options = new GUISearchOptions();
		options.setUserId(1L);
		options.setLanguage("en");
		options.setExpression("folder");
		options.setType(SearchOptions.TYPE_FOLDERS);
		options.setName("test2");
		GUICriterion crit = new GUICriterion();
		crit.setField("name");
		crit.setStringValue("folder");
		crit.setOperator(FolderCriterion.OPERATOR_CONTAINS);
		options.getCriteria().add(crit);
		crit = new GUICriterion();
		crit.setField("id");
		crit.setLongValue(1L);
		crit.setOperator(FolderCriterion.OPERATOR_GREATER);
		
		testSubject.save(options);

		loaded = testSubject.load("test2");
		assertNotNull(loaded);

		assertEquals(2, SearchServiceImpl.getSearches(session).size());

		testSubject.delete(List.of("test"));
		try {
			assertNotNull(testSubject.load("test"));
			fail("No error if loading a deleted search");
		} catch (ServerException se) {
			// All ok
		}
	}

	@Test
	public void testShareSearch() throws ServerException, PersistenceException {
		GUISearchOptions options = new GUISearchOptions();
		options.setUserId(1L);
		options.setLanguage("en");
		options.setExpression("document");
		options.setType(SearchOptions.TYPE_FULLTEXT);
		options.setName("test");

		testSubject.save(options);
		assertEquals(1, searchDao.findAll(Tenant.DEFAULT_ID).size());

		testSubject.shareSearch("test", List.of(3L, 4L), List.of(2L, 3L));
		assertEquals(4, searchDao.findAll(Tenant.DEFAULT_ID).size());
	}

	private void addHits() throws Exception {
		DocumentDAO documentDao = (DocumentDAO) context.getBean("DocumentDAO");
		Document document = documentDao.findById(1L);
		documentDao.initialize(document);

		TemplateDAO templateDao = (TemplateDAO) context.getBean("TemplateDAO");
		document.setTemplate(templateDao.findById(-1L));
		document.setTemplateId(-1L);
		document.setValue("source", "boh");
		documentDao.store(document);

		engine.addHit(document, "Questo e un documento di prova. Per fortuna che esistono i test. document");

		Folder fold = new Folder();
		fold.setId(Folder.DEFAULTWORKSPACEID);
		fold.setName("test");

		// Adding unexisting document 111
		document = new Document();
		document.setId(111L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response. document");

		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document, "Another document");

		document = new Document();
		document.setId(3L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"Lorem ipsum dolor sit amet, consectetur 5568299afbX0 ZKBKCHZZ80A CH8900761016116097873 adipisicing elit");
	}
}