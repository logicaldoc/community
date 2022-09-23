package com.logicaldoc.core.searchengine.saved;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.searchengine.folder.FolderCriterion;
import com.logicaldoc.core.searchengine.folder.FolderSearchOptions;
import com.logicaldoc.util.Context;

import junit.framework.Assert;

/**
 * Test case for {@link HibernateSearchDAO}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.6.1
 */
public class HibernateSearchDAOTest extends AbstractCoreTCase {

	// Instance under test
	private SearchDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateSearchDAO
		dao = (SearchDAO) context.getBean("SearchDAO");
	}

	@Test
	public void testFindByUserId() throws PersistenceException, IOException {
		FulltextSearchOptions opt = new FulltextSearchOptions();

		opt.setLanguage("it");
		opt.setExpression("prova test");
		opt.setExpressionLanguage("it");
		opt.setTemplate(1L);
		opt.setSizeMax(3000L);
		opt.setSizeMin(2L);
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);

		SavedSearch search = new SavedSearch();
		search.setName("search1");
		search.setUserId(1L);
		search.setTenantId(1L);
		search.saveOptions(opt);
		dao.store(search);

		search = new SavedSearch();
		search.setName("search2");
		search.setUserId(1L);
		search.setTenantId(1L);
		search.saveOptions(opt);
		dao.store(search);

		List<SavedSearch> searches = dao.findByUserId(1L);
		Assert.assertEquals(2, searches.size());
		Assert.assertEquals("search1", searches.get(0).getName());

		searches = dao.findByUserId(5L);
		Assert.assertEquals(0, searches.size());
	}

	@Test
	public void testCharsets() {
		FulltextSearchOptions opt = new FulltextSearchOptions();

		opt.setLanguage("it");
		opt.setExpression("ואתם נהנים");
		opt.setExpressionLanguage("it");
		opt.setTemplate(1L);
		opt.setSizeMax(3000L);
		opt.setSizeMin(2L);
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);

		opt.setCreationFrom(new Date());

		Map<String, Charset> charsets = Charset.availableCharsets();
		for (String name : charsets.keySet()) {
			try {

				Context.get().getProperties().setProperty("default.charset", name);
				SavedSearch saved = new SavedSearch();
				saved.setName("ואתם נהנים מהטבה ייחודית");
				saved.saveOptions(opt);
				String xml = saved.getOptions();

				saved = new SavedSearch();
				saved.setOptions(xml);
			} catch (Throwable t) {
			}
		}
	}

	@Test
	public void testFolders() throws IOException {
		FolderSearchOptions opt = new FolderSearchOptions();

	
		opt.setExpression("ואתם נהנים");
		opt.setTemplate(1L);
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);


		FolderCriterion crit=new FolderCriterion();
		crit.setField("pippo type:3");
		crit.setDateValue(new Date());
		opt.getCriteria().add(crit);
		
		
		SavedSearch saved = new SavedSearch();
		saved.setName("ואתם נהנים מהטבה ייחודית");
		saved.saveOptions(opt);
		String xml = saved.getOptions();	
	}
}