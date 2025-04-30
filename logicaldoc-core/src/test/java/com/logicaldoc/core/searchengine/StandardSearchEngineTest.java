package com.logicaldoc.core.searchengine;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Date;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

public class StandardSearchEngineTest extends AbstractCoreTestCase {

	protected SearchEngine testSubject;

	protected DocumentDAO documentDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = Context.get(SearchEngine.class);
		documentDao = Context.get(DocumentDAO.class);
	}

	@After
	@Override
	public void tearDown() throws SQLException, IOException {
		testSubject.unlock();
		testSubject.close();
		super.tearDown();
	}

	@Test
	public void testAddHit() throws Exception {
		Document document = new Document();
		document.setId(1L);
		document.setFileName("Document test 1");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold = new Folder();
		fold.setId(Folder.DEFAULTWORKSPACEID);
		fold.setName("test");
		document.setFolder(fold);

		testSubject.unlock();
		documentDao.initialize(document);
		testSubject.addHit(document, "Questo è un documento di prova. Per fortuna che esistono i test. document");

		Hit hit = testSubject.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		document = new Document();
		document.setId(111L);
		document.setFileName("Document test 111");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response. document.");

		hit = testSubject.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		hit = testSubject.getHit(111L);
		Assert.assertEquals(111L, hit.getId());

		Assert.assertEquals(2, testSubject.getCount());

		hit = testSubject.getHit(112L);
		Assert.assertNull(hit);
	}

	@Test
	public void testDeleteHit() throws Exception {
		testAddHit();
		Hit hit = testSubject.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		testSubject.deleteHit(1L);

		hit = testSubject.getHit(1L);
		Assert.assertNull(hit);

		testSubject.deleteHit(99L);

		Assert.assertEquals(1, testSubject.getCount());
	}

	@Test
	public void testDeleteHits() throws Exception {
		testAddHit();
		Assert.assertEquals(2L, testSubject.getCount());

		testSubject.deleteHits(Arrays.asList(new Long[] { 1L, 2L }));

		Assert.assertEquals(1L, testSubject.getCount());

		testSubject.deleteHits(Arrays.asList(new Long[] { 1L, 111L }));

		Assert.assertEquals(0L, testSubject.getCount());
	}

	@Test
	public void testQuery() throws Exception {
		testAddHit();

		Document document = new Document();
		document.setId(1L);
		document.setFileName("Document test 1");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold = new Folder();
		fold.setId(Folder.DEFAULTWORKSPACEID);
		fold.setName("test");
		document.setFolder(fold);

		document.setId(200L);
		document.setFileName("Document test 200");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document, "This test 200");

		document = new Document();
		document.setId(201L);
		document.setFileName("Document test 201");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		documentDao.initialize(document);
		testSubject.addHit(document, "This test 201");

		Hits hits = testSubject.query("*:*", 2, 3);
		Assert.assertEquals(1, hits.getCount());
	}

	@Test
	public void testSearch() throws Exception {
		testAddHit();
		Hits hits = testSubject.search("content:document", null, "en", 50);
		Assert.assertEquals(2, hits.getCount());

		hits = testSubject.search("content:document", null, "en", 1);
		Assert.assertEquals(1, hits.getCount());
		Assert.assertEquals(2, hits.getEstimatedCount());

		hits = testSubject.search("content:document", Set.of("folderId:4", "date:[2012-01-01T00:00:00Z TO *]"), "en",
				50);

		Assert.assertEquals(1, hits.getCount());
		Assert.assertEquals(111L, hits.next().getId());

		hits = testSubject.search("content:document", Set.of("templateId:1"), "en", 50);
		Assert.assertEquals(0, hits.getCount());

		hits = testSubject.search("content:house", null, "en", 50);
		Assert.assertEquals(0, hits.getCount());
	}

	@Test
	public void testClose() throws Exception {
		Document document = new Document();
		document.setId(1L);
		document.setFileName("Document test 1");
		document.setLanguage("en");
		documentDao.initialize(document);
		testSubject.addHit(document, "This is a test content just for test insertion");

		Hit hit = testSubject.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		testSubject.close();

		hit = testSubject.getHit(1L);
		Assert.assertNull(hit);
	}

	@Test
	public void testDropIndex() throws Exception {
		testAddHit();
		testSubject.dropIndex();
		Hit hit = testSubject.getHit(1L);
		Assert.assertNull(hit);
	}

	@Test
	public void testPurge() throws Exception {
		testAddHit();
	}
}