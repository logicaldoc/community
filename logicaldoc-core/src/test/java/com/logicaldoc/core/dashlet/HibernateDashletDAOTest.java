package com.logicaldoc.core.dashlet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateDashletDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class HibernateDashletDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DashletDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDashletDAO
		testSubject = Context.get(DashletDAO.class);
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Dashlet dashlet = testSubject.findByName("checkout", 1L);
		assertNotNull(dashlet);
		assertEquals(1L, dashlet.getId());

		dashlet = testSubject.findByName("xxxx", 1L);
		assertNull(dashlet);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Dashlet dashlet = retrieveDashlet().get(9);
		assertNotNull(dashlet);		
		testSubject.delete(dashlet.getId(), 1);
	}

	@Test
	public void testDashletHashCodeAndEquals() throws PersistenceException {
		Dashlet dashlet1 = retrieveDashlet().get(0);
		Dashlet dashlet2 = retrieveDashlet().get(1);

		assertNotSame(dashlet1.hashCode(), dashlet2.hashCode());

		assertEquals(dashlet1, dashlet1);
		assertEquals(false, dashlet1.equals(dashlet2));

		assertEquals(false, dashlet1.equals(new Object()));

		dashlet2.setId(dashlet1.getId());
		assertEquals(false, dashlet1.equals(dashlet2));

		dashlet1.setName(null);
		assertEquals(false, dashlet1.equals(dashlet2));
	}
	
	private List<Dashlet> retrieveDashlet() throws PersistenceException {
		TenantDAO tenantDao = Context.get(TenantDAO.class);
		Tenant tenant = tenantDao.findById(1L);
		assertNotNull(tenant);

		List<Dashlet> dashlets = testSubject.findAll(Tenant.DEFAULT_ID);
		for (Dashlet dashlet : dashlets) {
			Dashlet newDashlet = new Dashlet(dashlet);
			assertNotNull(newDashlet);
			assertNotSame(null, newDashlet.toString());
		}
		return dashlets;
	}
}
