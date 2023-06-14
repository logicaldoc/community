package com.logicaldoc.core.dashlet;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateDashletDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class HibernateDashletDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DashletDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDashletDAO
		dao = (DashletDAO) context.getBean("DashletDAO");
	}

	@Test
	public void testFindByName() {
		Dashlet dashlet = dao.findByName("checkout", 1L);
		Assert.assertNotNull(dashlet);
		Assert.assertEquals(1L, dashlet.getId());
		
		dashlet = dao.findByName("xxxx", 1L);
		Assert.assertNull(dashlet);
	}
}
