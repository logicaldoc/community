package com.logicaldoc.core.ticket;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link HibernateTicketDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateTicketDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private TicketDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDownaloadTicketoDAO
		testSubject = TicketDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		Ticket ticket = testSubject.findByTicketId("1");
		assertNotNull(ticket);
		assertTrue(testSubject.deleteByTicketId("1"));
		ticket = testSubject.findByTicketId("1");
		assertNull(ticket);
	}

	@Test
	public void testDeleteByDocId() throws PersistenceException {
		Ticket ticket = testSubject.findByTicketId("1");
		assertNotNull(ticket);
		assertEquals(1, ticket.getDocId());
		ticket = testSubject.findByTicketId("3");
		assertNotNull(ticket);
		assertEquals(1, ticket.getDocId());

		assertTrue(testSubject.deleteByDocId(1));
		ticket = testSubject.findByTicketId("1");
		assertNull(ticket);
		ticket = testSubject.findByTicketId("3");
		assertNull(ticket);
	}

	@Test
	public void testDeleteExpired() throws PersistenceException {
		List<Ticket> tickets = testSubject.findAll();
		assertEquals(3, tickets.size());

		testSubject.deleteExpired();
		tickets = testSubject.findAll();
		assertEquals(0, tickets.size());
	}

	@Test
	public void testFindByTicketId() throws PersistenceException {
		Ticket ticket = testSubject.findByTicketId("1");
		assertNotNull(ticket);
		assertEquals(1, ticket.getUserId());
		assertEquals(1, ticket.getDocId());

		ticket = testSubject.findByTicketId("99");
		assertNull(ticket);
	}

	@Test
	public void testFindById() throws PersistenceException {
		Ticket ticket = testSubject.findById(1);
		assertNotNull(ticket);
		assertEquals(1, ticket.getUserId());
		assertEquals(1, ticket.getDocId());

		ticket = testSubject.findById(99);
		assertNull(ticket);
	}

	@Test
	public void testStore() throws PersistenceException, NoSuchAlgorithmException {
		Ticket ticket = new Ticket();
		ticket.setDocId(1);
		ticket.setUserId(3);
		ticket.setTicketId("5");
		ticket.setDecodedPassword("1234ABC");
		testSubject.store(ticket);

		Ticket storedTicket = testSubject.findByTicketId("5");
		assertNotNull(storedTicket);
		assertEquals(ticket, storedTicket);
		assertEquals(CryptUtil.encryptSHA256("1234ABC"), storedTicket.getPassword());
	}

	@Test
	public void testCount() throws PersistenceException, NoSuchAlgorithmException {
		assertEquals(3L, testSubject.countViewOrDownloadTickets(null));
		assertEquals(3L, testSubject.countViewOrDownloadTickets(Tenant.SYSTEM_ID));

		Ticket ticket = new Ticket();
		ticket.setDocId(1);
		ticket.setUserId(3);
		ticket.setTenantId(2L);
		ticket.setType(Ticket.VIEW);
		ticket.setTicketId("88");
		testSubject.store(ticket);

		assertEquals(4L, testSubject.countViewOrDownloadTickets(Tenant.SYSTEM_ID));
		assertEquals(1L, testSubject.countViewOrDownloadTickets(2L));
		assertEquals(3L, testSubject.countViewOrDownloadTickets(Tenant.DEFAULT_ID));
	}
}