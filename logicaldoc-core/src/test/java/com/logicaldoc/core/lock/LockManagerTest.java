package com.logicaldoc.core.lock;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.config.ContextProperties;

import junit.framework.Assert;

/**
 * Test case for <code>LockManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class LockManagerTest extends AbstractCoreTestCase {
	private LockManager manager;

	private GenericDAO dao;

	private ContextProperties config;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		manager = (LockManager) context.getBean("LockManager");
		dao = (GenericDAO) context.getBean("GenericDAO");
		config = (ContextProperties) context.getBean("ContextProperties");
	}

	@Test
	public void testGet() throws PersistenceException {
		Assert.assertTrue(manager.get("test", "t1"));
		Assert.assertTrue(manager.get("test", "t1"));
		Assert.assertFalse(manager.get("test", "t2"));
		
		synchronized (this) {
			try {
				wait(3000);
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}

		Assert.assertTrue(manager.get("test", "t2"));
		manager.release("test", "t2");

		Generic lock = dao.findByAlternateKey("lock", "test-" + config.getProperty("id"), null, Tenant.DEFAULT_ID);
		Assert.assertNotNull(lock);
		Assert.assertNull(lock.getString1());
		Assert.assertNull(lock.getDate1());
	}
}
