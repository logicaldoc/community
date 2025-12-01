package com.logicaldoc.core.sequence;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;

public class HibernateSequenceDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private SequenceDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = SequenceDAO.get();
	}

	@Test
	public void testReset() throws PersistenceException {
		testSubject.reset("test", 0L, Tenant.DEFAULT_ID, 5);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 5, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		testSubject.reset("test", 0L, Tenant.DEFAULT_ID, 100);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 100, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		testSubject.reset("test2", 0L, Tenant.DEFAULT_ID, 30);
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i + 30, testSubject.next("test2", 0L, Tenant.DEFAULT_ID));
		}
	}

	@Test
	public void testNext() throws PersistenceException {
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		for (int i = 1; i <= 20; i++) {
			Assert.assertEquals(i, testSubject.next("test2", 0L, Tenant.DEFAULT_ID));
		}
		Assert.assertEquals(25L, testSubject.next("test2", 0L, Tenant.DEFAULT_ID, 5L));
		Assert.assertEquals(23L, testSubject.next("test2", 0L, Tenant.DEFAULT_ID, -2L));
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Collection<Sequence> sequences = testSubject.findByName("customid-", Tenant.DEFAULT_ID);
		Assert.assertNotNull(sequences);
		Assert.assertEquals(2, sequences.size());
	}
}