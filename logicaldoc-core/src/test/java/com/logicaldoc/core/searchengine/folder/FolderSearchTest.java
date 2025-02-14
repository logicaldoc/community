package com.logicaldoc.core.searchengine.folder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.searchengine.Hit;

import junit.framework.Assert;

public class FolderSearchTest extends AbstractCoreTestCase {

	private static final Logger log = LoggerFactory.getLogger(FolderSearchTest.class);

	@Test
	public void testSearch() {
		List<FolderCriterion> criteria = new ArrayList<>();

		FolderCriterion criterion = new FolderCriterion();
		criterion.setComposition("and");
		criterion.setOperator(FolderCriterion.OPERATOR_CONTAINS);
		criterion.setField("name");
		criterion.setValue("ABC");
		criteria.add(criterion);

		FolderSearchOptions opt = new FolderSearchOptions();
		opt.setCriteria(new ArrayList<>());
		opt.setMaxHits(10);
		opt.setUserId(1);
		opt.setRetrieveAliases(true);
		opt.setCriteria(criteria);
		opt.setOrder( Arrays.asList(new String[] { "lastmodified desc", "name asc" }));

		FolderSearch search = new FolderSearch();
		search.setOptions(opt);
		try {
			search.search();
		} catch (Exception e) {
			e.printStackTrace();
			log.error(e.getMessage(), e);
		}

		List<Hit> results = search.getHits();
		Assert.assertEquals(1, results.size());
		
		Assert.assertEquals(1201, results.get(0).getId());
		Assert.assertEquals(1200, results.get(0).getFolder().getId());

		criteria.clear();
		criterion = new FolderCriterion();
		criterion.setComposition("and");
		criterion.setExtendedAttribute(true);
		criterion.setOperator(FolderCriterion.OPERATOR_EQUALS);
		criterion.setField("val1");
		criterion.setValue("test_val_1");

		criterion = new FolderCriterion();
		criterion.setComposition("and");
		criterion.setOperator(FolderCriterion.OPERATOR_EQUALS);
		criterion.setField("template");
		criterion.setValue(1L);
		criterion.setType(FolderCriterion.TYPE_TEMPLATE);
		criteria.add(criterion);

		opt = new FolderSearchOptions();
		opt.setCriteria(new ArrayList<>());
		opt.setMaxHits(10);
		opt.setUserId(1);
		opt.setRetrieveAliases(true);
		opt.setCriteria(criteria);
		opt.setOrder(Arrays.asList(new String[] { "lastmodified desc", "name asc" }));

		search = new FolderSearch();
		search.setOptions(opt);
		try {
			search.search();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		results = search.getHits();
		Assert.assertEquals(2, results.size());
		Assert.assertEquals(1201L, results.get(0).getId());
	}
}