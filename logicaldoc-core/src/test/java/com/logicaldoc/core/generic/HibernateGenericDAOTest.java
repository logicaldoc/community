package com.logicaldoc.core.generic;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for {@link HibernateGenericDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateGenericDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private GenericDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateGenericDAO
		testSubject = Context.get(GenericDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Assert.assertNotNull(testSubject.findById(1L));
		testSubject.delete(1L);
		Assert.assertNull(testSubject.findById(1L));
	}

	@Test
	public void testFindByAlternateKey() throws PersistenceException {
		Generic generic = testSubject.findByAlternateKey("a", "a1", null, Tenant.DEFAULT_ID);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
		generic = testSubject.findByAlternateKey("a", "xxx", null, 99L);
		Assert.assertNull(generic);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Generic generic = testSubject.findById(1L);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
	}

	@Test
	public void testFindByTypeAndSubtype() throws PersistenceException {
		List<Generic> generics = testSubject.findByTypeAndSubtype("a", "a%", null, null);
		Assert.assertEquals(2, generics.size());
		generics = testSubject.findByTypeAndSubtype("a", "a%", null, Tenant.DEFAULT_ID);
		Assert.assertEquals(2, generics.size());
		generics = testSubject.findByTypeAndSubtype("a", "a%", null, 99L);
		Assert.assertEquals(0, generics.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		Generic generic = new Generic();
		generic.setType("xx");
		generic.setSubtype("xxx");
		generic.setInteger1(22L);
		generic.setString1("aaa");
		testSubject.store(generic);
		Assert.assertNotNull(generic);
		generic = testSubject.findById(generic.getId());
		Assert.assertEquals("xx", generic.getType());
		Assert.assertEquals("xxx", generic.getSubtype());
		Assert.assertEquals(Long.valueOf(22L), generic.getInteger1());
		Assert.assertEquals("aaa", generic.getString1());
	}

	@Test
	public void testInitialize() throws PersistenceException {
		Generic generic = testSubject.findById(1);
		Assert.assertNotNull(generic);
		Assert.assertEquals(Long.valueOf(0L), generic.getInteger1());
		testSubject.initialize(generic);
		Assert.assertEquals(1, generic.getAttributes().size());
		Assert.assertEquals("val1", generic.getValue("att1"));
	}
}