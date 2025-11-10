package com.logicaldoc.core.searchengine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.util.plugin.PluginException;

public class FulltextSearchTest extends AbstractCoreTestCase {

	private SearchEngine testSubject;

	private DocumentDAO documentDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = SearchEngine.get();
		documentDao = DocumentDAO.get();
		try {
			addHits();
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	@Test
	public void testWrite() throws IOException, ClassNotFoundException {
		File file = new File(tempDir, "query.ser");

		FulltextSearchOptions opt = new FulltextSearchOptions();

		opt.setLanguage("it");
		opt.setExpression("prova test");
		opt.setExpressionLanguage("it");
		opt.setTemplate(1L);
		opt.setSizeMax(3000L);
		opt.setSizeMin(2L);
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);

		opt.write(file);

		FulltextSearchOptions opt2 = (FulltextSearchOptions) SearchOptions.read(file);

		assertEquals("prova test", opt2.getExpression());
		assertEquals("it", opt2.getExpressionLanguage());
		assertEquals(1, opt2.getTemplate().longValue());
		assertEquals(3000, opt2.getSizeMax().longValue());
		assertEquals(2, opt2.getSizeMin().longValue());
		assertEquals(SearchOptions.TYPE_FULLTEXT, opt2.getType());
		assertEquals(1, opt2.getUserId());
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
		testSubject.addHit(document, "Questo e un documento di prova. Per fortuna che esistono i test. document");

		// Adding unexisting document 111
		document = new Document();
		document.setId(111L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response.");

		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document, "Another document");

		document = new Document();
		document.setId(3L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document,
				"Lorem ipsum dolor sit amet, consectetur 5568299afbX0 ZKBKCHZZ80A CH8900761016116097873 adipisicing elit");
	}

	@Test
	public void testSearch() throws Exception {
		assertEquals(4, testSubject.getCount());

		FulltextSearchOptions opt = new FulltextSearchOptions();
		opt.setLanguage("en");
		opt.setExpression("document");
		opt.setFields(Set.of("content", "title"));
		opt.setExpressionLanguage("en");
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);

		Search search = new FulltextSearch();
		search.setOptions(opt);

		List<Hit> hits = null;
		hits = search.search();
		assertEquals(2, hits.size());

		opt.setMaxHits(1);
		hits = search.search();
		assertEquals(1, hits.size());
		assertTrue(search.isMoreHitsPresent());

		opt = new FulltextSearchOptions();
		opt.setLanguage("en");
		opt.setExpression("CH8900761016116097873");
		opt.setFields(Set.of("content", "title"));
		opt.setExpressionLanguage("en");
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);

		search = new FulltextSearch();
		search.setOptions(opt);

		hits = search.search();
		assertEquals(1, hits.size());
		assertEquals(3L, hits.get(0).getId());
		assertEquals("en", hits.get(0).getLanguage());
	}

	@Test
	public void testSearchInFolder() throws Exception {
		FolderDAO folderDao = (FolderDAO) context.getBean("folderDAO");
		for (Long folderId : folderDao.findAllIds()) {
			folderDao.computePath(folderId);
		}

		assertEquals(4, testSubject.getCount());

		// Search in a tree
		FulltextSearchOptions opt = new FulltextSearchOptions();
		opt.setLanguage("en");
		opt.setExpression("document");
		opt.setFields(Set.of("content", "title"));
		opt.setExpressionLanguage("en");
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);
		opt.setFolderId(3000L);
		opt.setSearchInSubPath(true);

		Search search = new FulltextSearch();
		search.setOptions(opt);

		List<Hit> hits = search.search();
		assertEquals(2, hits.size());

		// Search in another tree
		opt.setFolderId(5L);
		opt.setSearchInSubPath(true);

		search = new FulltextSearch();
		search.setOptions(opt);

		hits = search.search();
		assertEquals(0, hits.size());

		// Search in single folder
		opt.setFolderId(3000L);
		opt.setSearchInSubPath(false);

		search = new FulltextSearch();
		search.setOptions(opt);

		hits = search.search();
		assertEquals(0, hits.size());

		// Search in single folder
		opt.setFolderId(6L);
		opt.setSearchInSubPath(false);

		search = new FulltextSearch();
		search.setOptions(opt);

		hits = search.search();
		assertEquals(2, hits.size());
	}
}