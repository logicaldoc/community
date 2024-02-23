package com.logicaldoc.core.security.user;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernatePasswordHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.6.1
 */
public class HibernatePasswordHistoryDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private PasswordHistoryDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernatePasswordHistoryDAO
		dao = (PasswordHistoryDAO) context.getBean("PasswordHistoryDAO");
	}

	@Test
	public void testCleanOldHistories() throws PersistenceException {
		List<PasswordHistory> histories = dao.findByUserId(1L, null);
		Assert.assertEquals(3, histories.size());

		dao.cleanOldHistories(1L, 2);

		histories = dao.findByUserId(1L, null);
		Assert.assertEquals(2, histories.size());
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		List<PasswordHistory> histories = dao.findByUserId(1L, null);
		Assert.assertEquals(3, histories.size());

		histories = dao.findByUserId(1L, 100);
		Assert.assertEquals(3, histories.size());

		histories = dao.findByUserId(1L, 2);
		Assert.assertEquals(2, histories.size());
		Assert.assertEquals("psw1", histories.get(0).getPassword());
	}
}