package com.logicaldoc.core.security.user;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.DateBean;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateUserHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class HibernateUserHistoryDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private UserHistoryDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateHistoryDAO
		dao = (UserHistoryDAO) context.getBean("UserHistoryDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		Collection<UserHistory> histories = (Collection<UserHistory>) dao.findByUserId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(2, histories.size());

		for (UserHistory history : histories)
			dao.delete(history.getId());

		histories = (Collection<UserHistory>) dao.findByUserId(4);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@Test
	public void testFindByUserIdAndType() {
		List<UserHistory> histories = dao.findByUserId(1);
		Assert.assertNotNull(histories);
		Assert.assertEquals(2, histories.size());

		histories = dao.findByUserIdAndEvent(1L, "data test 02");
		Assert.assertNotNull(histories);
		Assert.assertEquals(1, histories.size());
		Assert.assertEquals("data test 02", histories.get(0).getEvent());

		// Try with unexisting user
		histories = dao.findByUserId(99);
		Assert.assertNotNull(histories);
		Assert.assertEquals(0, histories.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		UserHistory userHistory = new UserHistory();
		userHistory.setDate(DateBean.dateFromCompactString("20061220"));
		userHistory.setUsername("sebastian");
		userHistory.setUserId(3L);
		userHistory.setEvent("first test User History store");

		dao.store(userHistory);
		Assert.assertNotNull(userHistory);

		UserHistory newUserHistory = new UserHistory();
		newUserHistory.setDate(DateBean.dateFromCompactString("20061220"));
		newUserHistory.setUsername("sebastian");
		newUserHistory.setUserId(3L);
		newUserHistory.setEvent("second test User History store");

		dao.store(newUserHistory);
		Assert.assertNotNull(newUserHistory);

		// Test the stored history
		Collection<UserHistory> histories = (Collection<UserHistory>) dao.findByUserId(3);
		Assert.assertNotNull(histories);
		Assert.assertFalse(histories.isEmpty());

		UserHistory hStored = null;
		for (UserHistory userHistory2 : histories) {
			if (userHistory2.getId() == newUserHistory.getId()) {
				hStored = userHistory2;
				break;
			}
		}

		Assert.assertEquals(hStored, newUserHistory);
		Assert.assertEquals(hStored.getDate().getTime(), DateBean.dateFromCompactString("20061220").getTime());
		Assert.assertEquals(hStored.getUsername(), "sebastian");
		Assert.assertEquals(hStored.getEvent(), "second test User History store");
	}

	@Test
	public void testCleanOldHistories() throws PersistenceException {
		dao.cleanOldHistories(5);

		UserHistory history = dao.findById(1);
		Assert.assertNull(history);
		List<UserHistory> histories = dao.findAll();
		Assert.assertEquals(0, histories.size());
	}
}