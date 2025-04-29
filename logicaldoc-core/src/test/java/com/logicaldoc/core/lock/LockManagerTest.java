package com.logicaldoc.core.lock;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for <code>LockManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class LockManagerTest extends AbstractCoreTestCase {

	private LockManager testSubject;

	private GenericDAO dao;

	private ContextProperties config;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(LockManager.class);
		dao = Context.get(GenericDAO.class);
		config = Context.get().getProperties();
	}

	@Test
	public void testGet() throws PersistenceException {
		Assert.assertTrue(testSubject.get("test", "t1"));
		Assert.assertTrue(testSubject.get("test", "t1"));
		Assert.assertFalse(testSubject.get("test", "t2"));

		synchronized (this) {
			try {
				wait(3000);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}

		Assert.assertTrue(testSubject.get("test", "t2"));
		testSubject.release("test", "t2");

		Generic lock = dao.findByAlternateKey("lock", "test-" + config.getProperty("id"), null, Tenant.DEFAULT_ID);
		Assert.assertNotNull(lock);
		Assert.assertNull(lock.getString1());
		Assert.assertNull(lock.getDate1());
	}
}
