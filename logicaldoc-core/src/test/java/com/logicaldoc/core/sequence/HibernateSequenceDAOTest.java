package com.logicaldoc.core.sequence;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;

public class HibernateSequenceDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private SequenceDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateSequqnceDAO
		dao = (SequenceDAO) context.getBean("SequenceDAO");
	}

	@Test
	public void testReset() {
		dao.reset("test", 0L, Tenant.DEFAULT_ID, 5);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 5, dao.next("test", 0L, Tenant.DEFAULT_ID));
		}
		dao.reset("test", 0L, Tenant.DEFAULT_ID, 100);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 100, dao.next("test", 0L, Tenant.DEFAULT_ID));
		}
		dao.reset("test2", 0L, Tenant.DEFAULT_ID, 30);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 30, dao.next("test2", 0L, Tenant.DEFAULT_ID));
		}
	}

	@Test
	public void testNext() {
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i, dao.next("test", 0L, Tenant.DEFAULT_ID));
		}
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i, dao.next("test2", 0L, Tenant.DEFAULT_ID));
		}
		Assert.assertEquals(25L, dao.next("test2", 0L, Tenant.DEFAULT_ID, 5L));
		Assert.assertEquals(23L, dao.next("test2", 0L, Tenant.DEFAULT_ID, -2L));
	}

	@Test
	public void testFindByName() {
		Collection<Sequence> sequences = dao.findByName("customid-", Tenant.DEFAULT_ID);
		Assert.assertNotNull(sequences);
		Assert.assertEquals(2, sequences.size());
	}
}