package com.logicaldoc.core.sequence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.concurrent.LaxSerialFuture;
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
			assertEquals(i + 5, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		testSubject.reset("test", 0L, Tenant.DEFAULT_ID, 100);
		for (int i = 1; i <= 20; i++) {
			assertEquals(i + 100, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		testSubject.reset("test2", 0L, Tenant.DEFAULT_ID, 30);
		for (int i = 1; i <= 20; i++) {
			assertEquals(i + 30, testSubject.next("test2", 0L, Tenant.DEFAULT_ID));
		}
	}

	@Test
	public void testNext() throws PersistenceException {
		for (int i = 1; i <= 20; i++) {
			assertEquals(i, testSubject.next("test", 0L, Tenant.DEFAULT_ID));
		}
		for (int i = 1; i <= 20; i++) {
			assertEquals(i, testSubject.next("test2", 0L, Tenant.DEFAULT_ID));
		}
		assertEquals(25L, testSubject.next("test2", 0L, Tenant.DEFAULT_ID, 5L));
		assertEquals(23L, testSubject.next("test2", 0L, Tenant.DEFAULT_ID, -2L));
	}

	@Test
	public void testNextHighConcurrency() throws PersistenceException, InterruptedException, ExecutionException {
		testSubject.reset("test", 0L, Tenant.DEFAULT_ID, 0);
		int totalThreads = 10000;
		List<Future<? extends Long>> futures = new ArrayList<>();
		for (int i = 0; i < totalThreads; i++) {
			Callable<Long> callable=new Callable<Long>() {
				
				@Override
				public Long call() throws Exception {
					testSubject.next("test", 0L, Tenant.DEFAULT_ID);
					testSubject.next("test", 0L, Tenant.DEFAULT_ID);
					return testSubject.next("test", 0L, Tenant.DEFAULT_ID);
				}
			};
			futures.add(ThreadPools.get().schedule(callable, "test", 1));
		}
		new LaxSerialFuture<>(futures).getAll();
		assertEquals(3 * totalThreads, testSubject.getCurrentValue("test", 0L, Tenant.DEFAULT_ID));
	}
	
	@Test
	public void testFindByName() throws PersistenceException {
		Collection<Sequence> sequences = testSubject.findByName("customid-", Tenant.DEFAULT_ID);
		assertNotNull(sequences);
		assertEquals(2, sequences.size());
	}
}