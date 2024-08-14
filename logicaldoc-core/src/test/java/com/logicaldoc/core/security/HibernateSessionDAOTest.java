package com.logicaldoc.core.security;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateSessionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class HibernateSessionDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private SessionDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateSessionDAO
		dao = (SessionDAO) context.getBean("SessionDAO");
	}

	@Test
	public void testDeleteCurrentNodeSessions() {
		Assert.assertEquals(1, dao.countSessions(1L, null));
		dao.deleteCurrentNodeSessions();
		Assert.assertEquals(0, dao.countSessions(1L, null));
	}

	@Test
	public void testFindByNode() {
		List<Session> sessions = dao.findByNode("saert536yy");
		Assert.assertEquals(1, sessions.size());
		sessions = dao.findByNode(null);
		Assert.assertEquals(1, sessions.size());
		sessions = dao.findByNode("xxxx");
		Assert.assertEquals(0, sessions.size());
	}

	@Test
	public void testFindBySid() {
		Session session = dao.findBySid("sid1");
		Assert.assertNotNull(session);
		Assert.assertEquals("addr1", session.getClient().getAddress());

		session = dao.findBySid("sid2");
		Assert.assertNull(session);
	}

	@Test
	public void testCountSessions() {
		Assert.assertEquals(1, dao.countSessions(1L, Session.STATUS_OPEN));
		Assert.assertEquals(1, dao.countSessions(null, Session.STATUS_OPEN));
		Assert.assertEquals(1, dao.countSessions(1L, null));
		Assert.assertEquals(1, dao.countSessions(null, null));

		Assert.assertEquals(0, dao.countSessions(1L, Session.STATUS_CLOSED));
		Assert.assertEquals(0, dao.countSessions(2L, Session.STATUS_OPEN));
	}
}