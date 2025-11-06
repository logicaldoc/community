package com.logicaldoc.core.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

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

		dao = SessionDAO.get();
	}

	@Test
	public void testDeleteCurrentNodeSessions() {
		assertEquals(1, dao.countSessions(1L, null));
		dao.deleteCurrentNodeSessions();
		assertEquals(0, dao.countSessions(1L, null));
	}

	@Test
	public void testFindByNode() {
		List<Session> sessions = dao.findByNode("saert536yy");
		assertEquals(1, sessions.size());
		sessions = dao.findByNode(null);
		assertEquals(1, sessions.size());
		sessions = dao.findByNode("xxxx");
		assertEquals(0, sessions.size());
	}

	@Test
	public void testFindBySid() {
		Session session = dao.findBySid("sid1");
		assertNotNull(session);
		assertEquals("addr1", session.getClient().getAddress());

		session = dao.findBySid("sid2");
		assertNull(session);
	}

	@Test
	public void testCountSessions() throws PersistenceException {
	 
		assertEquals(1, dao.countSessions(1L, Session.STATUS_OPEN));
		assertEquals(1, dao.countSessions((Long)null, Session.STATUS_OPEN));
		assertEquals(1, dao.countSessions(1L, null));
		assertEquals(1, dao.countSessions((Long)null, null));

		assertEquals(0, dao.countSessions(1L, Session.STATUS_CLOSED));
		assertEquals(0, dao.countSessions(2L, Session.STATUS_OPEN));
	
		System.out.println(""+dao.findAll().stream().map(s -> s.getUsername()+" "+s.getStatus()).toList());
		
		assertEquals(1, dao.countSessions((String)null, Session.STATUS_OPEN));
		assertEquals(1, dao.countSessions("admin", Session.STATUS_OPEN));
		assertEquals(0, dao.countSessions("joe", Session.STATUS_OPEN));
		
	}
}