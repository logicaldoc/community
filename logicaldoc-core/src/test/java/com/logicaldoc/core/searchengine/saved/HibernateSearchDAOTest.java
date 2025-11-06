package com.logicaldoc.core.searchengine.saved;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateSearchDAO}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.6.1
 */
public class HibernateSearchDAOTest extends AbstractCoreTestCase {

	private SearchDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = SearchDAO.get();
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
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
		testSubject.store(search);

		opt.setExpression("prova test 2");
		search = new SavedSearch();
		search.setName("search2");
		search.setUserId(1L);
		search.setTenantId(1L);
		search.saveOptions(opt);
		testSubject.store(search);

		List<SavedSearch> searches = testSubject.findByUserId(1L);
		assertEquals(2, searches.size());
		assertEquals("search1", searches.get(0).getName());

		searches = testSubject.findByUserId(5L);
		assertEquals(0, searches.size());
	}

	@Test
	public void testCharsets() throws PersistenceException {
		FulltextSearchOptions opt = new FulltextSearchOptions();

		opt.setLanguage("it");
		opt.setExpression("Cerco operai a 2000 euro al mese, nessuno risponde: manca l'umiltà");
		opt.setExpressionLanguage("it");
		opt.setTemplate(1L);
		opt.setSizeMax(3000L);
		opt.setSizeMin(2L);
		opt.setType(SearchOptions.TYPE_FULLTEXT);
		opt.setUserId(1);
		opt.setCreationFrom(new Date());

		Charset ch = Charset.forName("windows-1252");
		Context.get().getProperties().setProperty("default.charset", ch.name());
		SavedSearch saved = new SavedSearch();
		saved.setName("manca l'umiltà");
		saved.saveOptions(opt);
		String xml = saved.getOptions();
		assertNotNull(xml);
		assertTrue(xml.length() > 700);
		saved = new SavedSearch();
		saved.setOptions(xml);

		ch = StandardCharsets.UTF_8;
		Context.get().getProperties().setProperty("default.charset", ch.name());
		saved = new SavedSearch();
		saved.setName("manca l'umiltà");
		saved.saveOptions(opt);
		xml = saved.getOptions();
		assertNotNull(xml);
		assertTrue(xml.length() > 700);
		saved = new SavedSearch();
		saved.setOptions(xml);

		opt.setExpression("我正在寻找每月200欧元的工人，没有人回答：缺乏谦虚");
		ch = StandardCharsets.UTF_8;
		Context.get().getProperties().setProperty("default.charset", ch.name());
		saved = new SavedSearch();
		saved.setName("缺乏谦卑");
		saved.saveOptions(opt);
		xml = saved.getOptions();

		assertNotNull(xml);
		assertTrue(xml.length() > 600);

		saved = new SavedSearch();
		saved.setOptions(xml);
	}
}