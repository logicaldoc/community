package com.logicaldoc.core.security.user;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernatePasswordHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.6.1
 */
public class HibernatePasswordHistoryDAOTest extends AbstractCoreTestCase {

	private PasswordHistoryDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = PasswordHistoryDAO.get();
	}

	@Test
	public void testCleanOldHistories() throws PersistenceException {
		List<PasswordHistory> histories = testSubject.findByUserId(1L, null);
		assertEquals(3, histories.size());

		testSubject.cleanOldHistories(1L, 2);

		histories = testSubject.findByUserId(1L, null);
		assertEquals(2, histories.size());
	}

	@Test
	public void testFindByUserId() throws PersistenceException {
		List<PasswordHistory> histories = testSubject.findByUserId(1L, null);
		assertEquals(3, histories.size());

		histories = testSubject.findByUserId(1L, 100);
		assertEquals(3, histories.size());

		histories = testSubject.findByUserId(1L, 2);
		assertEquals(2, histories.size());
		assertEquals("psw1", histories.get(0).getPassword());
	}
}