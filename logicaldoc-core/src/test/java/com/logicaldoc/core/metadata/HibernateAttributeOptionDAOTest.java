package com.logicaldoc.core.metadata;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateAttributeOptionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class HibernateAttributeOptionDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private AttributeOptionDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateAttributeOptionDAO
		dao = (AttributeOptionDAO) context.getBean("AttributeOptionDAO");
	}

	@Test
	public void testDeleteByTemplateIdAndAttribute() throws PersistenceException {
		List<AttributeOption> options = dao.findByAttribute(1L, null);
		Assert.assertEquals(5, options.size());

		dao.deleteBySetIdAndAttribute(1L, "att1");
		options = dao.findByAttribute(1L, null);
		Assert.assertEquals(1, options.size());

		dao.deleteBySetIdAndAttribute(1L, null);
		options = dao.findByAttribute(1L, null);
		Assert.assertEquals(0, options.size());
	}

	@Test
	public void testDeleteOrphaned() throws PersistenceException {
		List<AttributeOption> options = dao.findByAttribute(1L, "att1");
		Assert.assertEquals(4, options.size());

		dao.deleteOrphaned(1L, (List<String>) Arrays.asList(new String[] { "pippo", "pluto" }));
		options = dao.findByAttribute(1L, "att1");
		Assert.assertEquals(0, options.size());
	}

	@Test
	public void testFindByTemplateAndAttribute() throws PersistenceException {
		List<AttributeOption> options = dao.findByAttribute(1L, "att1");
		Assert.assertEquals(4, options.size());
		options = dao.findByAttribute(1L, null);
		Assert.assertEquals(5, options.size());

		options = dao.findByAttribute(999L, null);
		Assert.assertEquals(0, options.size());

		options = dao.findByAttribute(1L, "att2");
		Assert.assertEquals(1, options.size());
		options = dao.findByAttribute(2L, "att1");
		Assert.assertEquals(1, options.size());
		options = dao.findByAttribute(2L, null);
		Assert.assertEquals(1, options.size());
	}

	@Test
	public void testFindBySetIdAndAttributeCategory() throws PersistenceException {
		List<AttributeOption> options = dao.findByAttributeAndCategory(1L, "att1", "cat1");
		Assert.assertEquals(3, options.size());
		Assert.assertEquals("value1", options.get(0).getValue());
		Assert.assertEquals("value2", options.get(1).getValue());
		Assert.assertEquals("value5", options.get(2).getValue());

		options = dao.findByAttributeAndCategory(1L, "att1", "cat2");
		Assert.assertEquals(1, options.size());
		Assert.assertEquals("value6", options.get(0).getValue());

		options = dao.findByAttributeAndCategory(1L, "att1", "catX");
		Assert.assertEquals(0, options.size());
	}

	@Test
	public void testFindByTemplateAndAttributeAsMap() throws PersistenceException {
		Map<String, List<AttributeOption>> optionsMap = dao.findByAttributeAsMap(1L, "att1");
		Assert.assertEquals(2, optionsMap.size());
		Assert.assertTrue(optionsMap.containsKey("cat1"));
		Assert.assertTrue(optionsMap.containsKey("cat2"));

		List<AttributeOption> options = optionsMap.get("cat1");
		Assert.assertEquals(3, options.size());
		Assert.assertEquals("value1", options.get(0).getValue());
		Assert.assertEquals("value2", options.get(1).getValue());
		Assert.assertEquals("value5", options.get(2).getValue());

		options = optionsMap.get("cat2");
		Assert.assertEquals(1, options.size());
		Assert.assertEquals("value6", options.get(0).getValue());

		optionsMap = dao.findByAttributeAsMap(1L, "pollo");
		Assert.assertEquals(0, optionsMap.size());
	}
}