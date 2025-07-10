package com.logicaldoc.core.ticket;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateTicketDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateTicketDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private TicketDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDownaloadTicketoDAO
		dao = Context.get(TicketDAO.class);
	}

	@Test
	public void testDelete() {
		Ticket ticket = dao.findByTicketId("1");
		assertNotNull(ticket);
		assertTrue(dao.deleteByTicketId("1"));
		ticket = dao.findByTicketId("1");
		assertNull(ticket);
	}

	@Test
	public void testDeleteByDocId() {
		Ticket ticket = dao.findByTicketId("1");
		assertNotNull(ticket);
		assertEquals(1, ticket.getDocId());
		ticket = dao.findByTicketId("3");
		assertNotNull(ticket);
		assertEquals(1, ticket.getDocId());

		assertTrue(dao.deleteByDocId(1));
		ticket = dao.findByTicketId("1");
		assertNull(ticket);
		ticket = dao.findByTicketId("3");
		assertNull(ticket);
	}

	@Test
	public void testDeleteExpired() throws PersistenceException {
		List<Ticket> tickets = dao.findAll();
		assertEquals(3, tickets.size());

		dao.deleteExpired();
		tickets = dao.findAll();
		assertEquals(0, tickets.size());
	}

	@Test
	public void testFindByTicketId() {
		Ticket ticket = dao.findByTicketId("1");
		assertNotNull(ticket);
		assertEquals(1, ticket.getUserId());
		assertEquals(1, ticket.getDocId());

		ticket = dao.findByTicketId("99");
		assertNull(ticket);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Ticket ticket = dao.findById(1);
		assertNotNull(ticket);
		assertEquals(1, ticket.getUserId());
		assertEquals(1, ticket.getDocId());

		ticket = dao.findById(99);
		assertNull(ticket);
	}

	@Test
	public void testStore() throws PersistenceException {
		Ticket ticket = new Ticket();
		ticket.setDocId(1);
		ticket.setUserId(3);
		ticket.setTicketId("5");
		dao.store(ticket);

		Ticket storedTicket = dao.findByTicketId("5");
		assertNotNull(storedTicket);
		assertEquals(ticket, storedTicket);
	}
}