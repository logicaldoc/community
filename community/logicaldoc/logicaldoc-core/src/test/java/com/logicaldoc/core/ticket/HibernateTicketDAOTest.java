package com.logicaldoc.core.ticket;

import java.text.ParseException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateTicketDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateTicketDAOTest extends AbstractCoreTCase {

	// Instance under test
	private TicketDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDownaloadTicketoDAO
		dao = (TicketDAO) context.getBean("TicketDAO");
	}

	@Test
	public void testDelete() {
		Ticket ticket = dao.findByTicketId("1");
		Assert.assertNotNull(ticket);
		Assert.assertTrue(dao.deleteByTicketId("1"));
		ticket = dao.findByTicketId("1");
		Assert.assertNull(ticket);
	}

	@Test
	public void testDeleteByDocId() {
		Ticket ticket = dao.findByTicketId("1");
		Assert.assertNotNull(ticket);
		Assert.assertEquals(1, ticket.getDocId());
		ticket = dao.findByTicketId("3");
		Assert.assertNotNull(ticket);
		Assert.assertEquals(1, ticket.getDocId());

		Assert.assertTrue(dao.deleteByDocId(1));
		ticket = dao.findByTicketId("1");
		Assert.assertNull(ticket);
		ticket = dao.findByTicketId("3");
		Assert.assertNull(ticket);
	}

	@Test
	public void testDeleteExpired() throws ParseException {
		List<Ticket> tickets = dao.findAll();
		Assert.assertEquals(3, tickets.size());

		dao.deleteExpired();
		tickets = dao.findAll();
		Assert.assertEquals(0, tickets.size());
	}

	@Test
	public void testFindByTicketId() {
		Ticket ticket = dao.findByTicketId("1");
		Assert.assertNotNull(ticket);
		Assert.assertEquals(1, ticket.getUserId());
		Assert.assertEquals(1, ticket.getDocId());

		ticket = dao.findByTicketId("99");
		Assert.assertNull(ticket);
	}

	@Test
	public void testFindById() {
		Ticket ticket = dao.findById(1);
		Assert.assertNotNull(ticket);
		Assert.assertEquals(1, ticket.getUserId());
		Assert.assertEquals(1, ticket.getDocId());

		ticket = dao.findById(99);
		Assert.assertNull(ticket);
	}

	@Test
	public void testStore() throws PersistenceException {
		Ticket ticket = new Ticket();
		ticket.setDocId(1);
		ticket.setUserId(3);
		ticket.setTicketId("5");
		dao.store(ticket);

		Ticket storedTicket = dao.findByTicketId("5");
		Assert.assertNotNull(storedTicket);
		Assert.assertEquals(ticket, storedTicket);
	}
}