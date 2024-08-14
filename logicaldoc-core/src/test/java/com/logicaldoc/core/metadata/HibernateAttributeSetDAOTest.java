package com.logicaldoc.core.metadata;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateAttributeDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateAttributeSetDAOTest extends AbstractCoreTestCase {

	private AttributeSetDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateAttributeSetDAO
		dao = (AttributeSetDAO) context.getBean("AttributeSetDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		dao.delete(1);
		AttributeSet set = dao.findById(1);
		Assert.assertNull(set);
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<AttributeSet> sets = dao.findAll();
		Assert.assertNotNull(sets);
		Assert.assertEquals(1, sets.size());
	}

	@Test
	public void testFindById() throws PersistenceException {
		AttributeSet set = dao.findById(-1);
		Assert.assertNotNull(set);
		Assert.assertEquals(-1, set.getId());
		Assert.assertEquals("default", set.getName());
		Assert.assertTrue(set.getAttributes().containsKey("object"));

		// Try with unexisting set
		set = dao.findById(99);
		Assert.assertNull(set);
	}

	@Test
	public void testFindByName() throws PersistenceException {
		AttributeSet set = dao.findByName("default", Tenant.DEFAULT_ID);
		Assert.assertNotNull(set);
		Assert.assertEquals(-1, set.getId());
		Assert.assertEquals("default", set.getName());

		set = dao.findByName("xxx", Tenant.DEFAULT_ID);
		Assert.assertNull(set);

		set = dao.findByName("default", 99L);
		Assert.assertNull(set);
	}

	@Test
	public void testStore() throws PersistenceException {
		AttributeSet set = new AttributeSet();
		set.setName("test3");
		set.setValue("a1", "v1");
		set.setValue("a2", "v2");
		dao.store(set);
		set = dao.findById(set.getId());
		Assert.assertEquals("test3", set.getName());
		Assert.assertTrue(set.getAttributes().containsKey("a1"));
		Assert.assertTrue(set.getAttributes().containsKey("a2"));
	}

	@Test
	public void testFindAttributes() throws PersistenceException {
		Map<String, Attribute> attributes = dao.findAttributes(1L, null);
		Assert.assertEquals(9, attributes.size());
		Assert.assertTrue(attributes.containsKey("sourceAuthor"));
	}
}