package com.logicaldoc.core.generic;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateGenericDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateGenericDAOTest extends AbstractCoreTCase {

	// Instance under test
	private GenericDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateGenericDAO
		dao = (GenericDAO) context.getBean("GenericDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		Assert.assertNotNull(dao.findById(1L));
		dao.delete(1L);
		Assert.assertNull(dao.findById(1L));
	}

	@Test
	public void testFindByAlternateKey() {
		Generic generic = dao.findByAlternateKey("a", "a1", null, Tenant.DEFAULT_ID);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
		generic = dao.findByAlternateKey("a", "xxx", null, 99L);
		Assert.assertNull(generic);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Generic generic = dao.findById(1L);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
	}

	@Test
	public void testFindByTypeAndSubtype() {
		List<Generic> generics = dao.findByTypeAndSubtype("a", "a%", null, null);
		Assert.assertEquals(2, generics.size());
		generics = dao.findByTypeAndSubtype("a", "a%", null, Tenant.DEFAULT_ID);
		Assert.assertEquals(2, generics.size());
		generics = dao.findByTypeAndSubtype("a", "a%", null, 99L);
		Assert.assertEquals(0, generics.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		Generic generic = new Generic();
		generic.setType("xx");
		generic.setSubtype("xxx");
		generic.setInteger1(22L);
		generic.setString1("aaa");
		Assert.assertTrue(dao.store(generic));
		generic = dao.findById(generic.getId());
		Assert.assertEquals("xx", generic.getType());
		Assert.assertEquals("xxx", generic.getSubtype());
		Assert.assertEquals(Long.valueOf(22L), generic.getInteger1());
		Assert.assertEquals("aaa", generic.getString1());
	}

	@Test
	public void testInitialize() throws PersistenceException {
		Generic generic = dao.findById(1);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
		dao.initialize(generic);
		Assert.assertEquals(1, generic.getAttributes().size());
		Assert.assertEquals("val1", generic.getValue("att1"));
	}
}