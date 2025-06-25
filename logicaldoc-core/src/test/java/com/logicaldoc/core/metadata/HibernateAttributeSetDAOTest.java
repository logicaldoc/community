package com.logicaldoc.core.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link HibernateAttributeDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateAttributeSetDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private AttributeSetDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		/*
		 * Retrieve the instance under test from spring context. Make sure that
		 * it is an HibernateAttributeSetDAO
		 */
		testSubject = Context.get(AttributeSetDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		testSubject.delete(1);
		AttributeSet set = testSubject.findById(1);
		assertNull(set);
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<AttributeSet> sets = testSubject.findAll();
		assertNotNull(sets);
		assertEquals(1, sets.size());
	}

	@Test
	public void testFindById() throws PersistenceException {
		AttributeSet set = testSubject.findById(-1);
		assertNotNull(set);
		testSubject.initialize(set);
		assertEquals(-1, set.getId());
		assertEquals("default", set.getName());
		assertTrue(set.getAttributes().containsKey("object"));

		// Try with non-existent set
		set = testSubject.findById(99);
		assertNull(set);
	}

	@Test
	public void testFindByName() throws PersistenceException {
		AttributeSet set = testSubject.findByName("default", Tenant.DEFAULT_ID);
		assertNotNull(set);
		assertEquals(-1, set.getId());
		assertEquals("default", set.getName());

		set = testSubject.findByName("xxx", Tenant.DEFAULT_ID);
		assertNull(set);

		set = testSubject.findByName("default", 99L);
		assertNull(set);
	}

	@Test
	public void testStore() throws PersistenceException {
		AttributeSet set = new AttributeSet();
		set.setName("test3");
		set.setValue("a1", "v1");
		set.setValue("a2", "v2");
		testSubject.store(set);
		set = testSubject.findById(set.getId());
		assertEquals("test3", set.getName());
		testSubject.initialize(set);
		assertTrue(set.getTemplateAttributes().containsKey("a1"));
		assertTrue(set.getAttributes().containsKey("a2"));
	}

	@Test
	public void testFindAttributes() throws PersistenceException {
		Map<String, Attribute> attributes = testSubject.findAttributes(1L, null);
		assertEquals(9, attributes.size());
		assertTrue(attributes.containsKey("sourceAuthor"));
	}
}