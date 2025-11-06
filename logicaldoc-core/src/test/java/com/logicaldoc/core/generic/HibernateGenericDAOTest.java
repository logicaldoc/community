package com.logicaldoc.core.generic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;

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
		testSubject = GenericDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		assertNotNull(testSubject.findById(1L));
		testSubject.delete(1L);
		assertNull(testSubject.findById(1L));
	}

	@Test
	public void testFindByAlternateKey() throws PersistenceException {
		Generic generic = testSubject.findByAlternateKey("a", "a1", null, Tenant.DEFAULT_ID);
		assertNotNull(generic);
		assertEquals(Long.valueOf(0L), generic.getInteger1());
		generic = testSubject.findByAlternateKey("a", "xxx", null, 99L);
		assertNull(generic);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Generic generic = testSubject.findById(1L);
		assertNotNull(generic);
		assertEquals(Long.valueOf(0L), generic.getInteger1());
	}

	@Test
	public void testFindByTypeAndSubtype() throws PersistenceException {
		List<Generic> generics = testSubject.findByTypeAndSubtype("a", "a%", null, null);
		assertEquals(2, generics.size());
		generics = testSubject.findByTypeAndSubtype("a", "a%", null, Tenant.DEFAULT_ID);
		assertEquals(2, generics.size());
		generics = testSubject.findByTypeAndSubtype("a", "a%", null, 99L);
		assertEquals(0, generics.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		Generic generic = new Generic();
		generic.setType("xx");
		generic.setSubtype("xxx");
		generic.setInteger1(22L);
		generic.setString1("aaa");
		testSubject.store(generic);
		assertNotNull(generic);
		generic = testSubject.findById(generic.getId());
		assertEquals("xx", generic.getType());
		assertEquals("xxx", generic.getSubtype());
		assertEquals(Long.valueOf(22L), generic.getInteger1());
		assertEquals("aaa", generic.getString1());
	}

	@Test
	public void testInitialize() throws PersistenceException {
		Generic generic = testSubject.findById(1);
		assertNotNull(generic);
		assertEquals(Long.valueOf(0L), generic.getInteger1());
		testSubject.initialize(generic);
		assertEquals(1, generic.getAttributes().size());
		assertEquals("val1", generic.getValue("att1"));
	}
}