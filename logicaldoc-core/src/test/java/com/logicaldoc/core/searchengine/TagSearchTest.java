package com.logicaldoc.core.searchengine;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;

public class TagSearchTest extends AbstractCoreTestCase {

	protected static Logger log = LoggerFactory.getLogger(TagSearchTest.class);

	@Test
	public void testSearch() {
		SearchOptions opt = new SearchOptions();
		opt.setType(SearchOptions.TYPE_TAG);
		opt.setUserId(1);
		opt.setExpression("abc");

		TagSearch search = new TagSearch();
		search.setOptions(opt);
		try {
			search.search();
		} catch (Exception e) {
			e.printStackTrace();
			log.error(e.getMessage(), e);
		}

		List<Hit> results = search.getHits();

		assertEquals(2, results.size());
		assertEquals(1, results.get(0).getId());

		opt = new SearchOptions();
		opt.setType(SearchOptions.TYPE_TAG);
		opt.setUserId(1);
		opt.setExpression("abc");
		opt.setMaxHits(1);

		search = new TagSearch();
		search.setOptions(opt);
		try {
			search.search();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		// We should get 2 entries because document 2 is an alias
		results = search.getHits();
		assertEquals(2, results.size());
		assertEquals(1, results.get(0).getId());
	}
}