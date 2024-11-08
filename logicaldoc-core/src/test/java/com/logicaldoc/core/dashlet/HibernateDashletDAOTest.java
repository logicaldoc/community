package com.logicaldoc.core.dashlet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateDashletDAO</code>
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
		testSubject = (DashletDAO) context.getBean("DashletDAO");
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Dashlet dashlet = testSubject.findByName("checkout", 1L);
		assertNotNull(dashlet);
		assertEquals(1L, dashlet.getId());

		dashlet = testSubject.findByName("xxxx", 1L);
		assertNull(dashlet);
	}
}
