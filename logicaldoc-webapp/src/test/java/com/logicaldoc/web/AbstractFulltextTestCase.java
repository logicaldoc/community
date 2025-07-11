package com.logicaldoc.web;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Before;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.saved.SearchDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

public abstract class AbstractFulltextTestCase extends AbstractWebappTestCase {

	protected SearchEngine engine;

	protected SearchDAO searchDao;

	public AbstractFulltextTestCase() {
		super();
	}

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		engine = Context.get(SearchEngine.class);
		searchDao = Context.get(SearchDAO.class);
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

	private void addHits() throws Exception {
		DocumentDAO documentDao = (DocumentDAO) context.getBean("documentDAO");
		Document document = documentDao.findById(1L);
		documentDao.initialize(document);

		TemplateDAO templateDao = Context.get(TemplateDAO.class);
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