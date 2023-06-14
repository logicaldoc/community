package com.logicaldoc.core.searchengine;

import java.sql.SQLException;
import java.util.Arrays;
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.Folder;

import junit.framework.Assert;

public class StandardSearchEngineTest extends AbstractCoreTestCase {

	protected static Logger log = LoggerFactory.getLogger(StandardSearchEngineTest.class);

	protected SearchEngine engine;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		engine = (SearchEngine) context.getBean("SearchEngine");
	}

	@After
	@Override
	public void tearDown() throws SQLException  {
		engine.unlock();
		engine.close();
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

		engine.unlock();
		engine.addHit(document, "Questo è un documento di prova. Per fortuna che esistono i test. document");

		Hit hit = engine.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		document = new Document();
		document.setId(111L);
		document.setFileName("Document test 111");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response. document.");
		
		hit = engine.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		hit = engine.getHit(111L);
		Assert.assertEquals(111L, hit.getId());

		Assert.assertEquals(2, engine.getCount());

		hit = engine.getHit(112L);
		Assert.assertNull(hit);
	}

	@Test
	public void testDeleteHit() throws Exception {
		testAddHit();
		Hit hit = engine.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		engine.deleteHit(1L);

		hit = engine.getHit(1L);
		Assert.assertNull(hit);

		engine.deleteHit(99L);

		Assert.assertEquals(1, engine.getCount());
	}

	@Test
	public void testDeleteHits() throws Exception {
		testAddHit();
		Assert.assertEquals(2L, engine.getCount());

		engine.deleteHits(Arrays.asList(new Long[] { 1L, 2L }));

		Assert.assertEquals(1L, engine.getCount());

		engine.deleteHits(Arrays.asList(new Long[] { 1L, 111L }));

		Assert.assertEquals(0L, engine.getCount());
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
		engine.addHit(document, "This test 200");

		
		document = new Document();
		document.setId(201L);
		document.setFileName("Document test 201");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document, "This test 201");
		
		Hits hits = engine.query("*:*", 2, 3);
		Assert.assertEquals(1, hits.getCount());
	}

	@Test
	public void testSearch() throws Exception {
		testAddHit();
		Hits hits = engine.search("content:document", null, "en", 50);
		Assert.assertEquals(2, hits.getCount());

		hits = engine.search("content:document", null, "en", 1);
		Assert.assertEquals(1, hits.getCount());
		Assert.assertEquals(2, hits.getEstimatedCount());

		hits = engine.search("content:document",
				new String[] { "templateId:0", "folderId:4", "date:[2012-01-01T00:00:00Z TO *]" }, "en", 50);
		Assert.assertEquals(1, hits.getCount());
		Assert.assertEquals(111L, hits.next().getId());

		hits = engine.search("content:document", new String[] { "templateId:1" }, "en", 50);
		Assert.assertEquals(0, hits.getCount());

		hits = engine.search("content:house", null, "en", 50);
		Assert.assertEquals(0, hits.getCount());
	}

	@Test
	public void testClose() throws Exception {
		Document document = new Document();
		document.setId(1L);
		document.setFileName("Document test 1");
		document.setLanguage("en");

		engine.addHit(document, "This is a test content just for test insertion");

		Hit hit = engine.getHit(1L);
		Assert.assertEquals(1L, hit.getId());
		Assert.assertEquals("en", hit.getLanguage());

		engine.close();

		hit = engine.getHit(1L);
		Assert.assertNull(hit);
	}

	@Test
	public void testDropIndex() throws Exception {
		testAddHit();
		engine.dropIndex();
		Hit hit = engine.getHit(1L);
		Assert.assertNull(hit);
	}

	@Test
	public void testPurge() throws Exception {
		testAddHit();
	}
}