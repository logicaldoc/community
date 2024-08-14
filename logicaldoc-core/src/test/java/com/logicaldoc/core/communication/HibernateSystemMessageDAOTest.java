package com.logicaldoc.core.communication;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateSystemMessageDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateSystemMessageDAOTest extends AbstractCoreTestCase {
	// Instance under test
	private SystemMessageDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context.
		// Make sure that it is an HibernateSystemMessageDAO
		dao = (SystemMessageDAO) context.getBean("SystemMessageDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		dao.delete(1);
		SystemMessage message = dao.findById(1);
		assertNull(message);
	}

	@Test
	public void testFindById() throws PersistenceException {
		SystemMessage message = dao.findById(1);
		assertNotNull(message);
		assertEquals(1, message.getId());
		assertEquals("message text1", message.getMessageText());

		// Try with unexisting message
		message = dao.findById(99);
		assertNull(message);
	}

	@Test
	public void testFindByRecipient() throws PersistenceException {
		Collection<SystemMessage> coll = dao.findByRecipient("sebastian", Message.TYPE_SYSTEM, null);
		assertEquals(1, coll.size());
		coll = dao.findByRecipient("marco", 1, null);
		assertEquals(2, coll.size());
		coll = dao.findByRecipient("paperino", 2, null);
		assertEquals(0, coll.size());
		coll = dao.findByRecipient("xxxx", Message.TYPE_SYSTEM, null);
		assertEquals(0, coll.size());
	}

	@Test
	public void testDeleteExpiredMessages() throws PersistenceException {
		dao.deleteExpiredMessages("sebastian");
		assertNotNull(dao.findById(1));

		dao.deleteExpiredMessages(Message.TYPE_SYSTEM);
		assertNotNull(dao.findById(1));
	}

	@Test
	public void testGetUnreadCount() throws PersistenceException {
		assertEquals(1, dao.getUnreadCount("sebastian", Message.TYPE_SYSTEM));
		assertEquals(2, dao.getUnreadCount("marco", 1));
		assertEquals(0, dao.getUnreadCount("admin", Message.TYPE_SYSTEM));
	}

	@Test
	public void testFindByType() throws PersistenceException {
		Collection<SystemMessage> coll = dao.findByType(0);
		assertEquals(2, coll.size());
		coll = dao.findByType(1);
		assertEquals(2, coll.size());
		coll = dao.findByType(2);
		assertEquals(1, coll.size());
		coll = dao.findByType(3);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindByMode() throws PersistenceException {
		Collection<SystemMessage> coll = dao.findByMode("CC");
		assertEquals(1, coll.size());
		coll = dao.findByMode("sms");
		assertEquals(2, coll.size());
		coll = dao.findByMode("socket");
		assertEquals(0, coll.size());
		coll = dao.findByMode("xxxx");
		assertEquals(0, coll.size());
	}

	@Test
	public void testStore() throws PersistenceException {
		Set<Recipient> recipients = new HashSet<>();
		Recipient recipient = new Recipient();
		recipient.setName("pippo");
		recipient.setAddress("pippo");
		recipient.setType(Recipient.TYPE_SYSTEM);
		recipient.setMode("test1");
		recipients.add(recipient);
		recipient = new Recipient();
		recipient.setName("paperino");
		recipient.setAddress("paperino");
		recipient.setType(Recipient.TYPE_EMAIL);
		recipient.setMode("test2");
		recipients.add(recipient);

		SystemMessage message = new SystemMessage();
		message.setAuthor("admin");
		message.setMessageText("text");
		message.setLastNotified(new Date());
		message.setType(Message.TYPE_SYSTEM);
		message.setStatus(SystemMessage.STATUS_NEW);
		message.setRecipients(recipients);
		dao.store(message);
		assertNotNull(message);
		message = dao.findById(message.getId());
		assertNotNull(message);
		assertEquals(2, message.getRecipients().size());

		// Update an already existing message
		message = dao.findById(1);
		assertNotNull(message);
		assertEquals("message text1", message.getMessageText());
		message.setMessageText("xxxx");
		message.setRecipients(recipients);
		dao.store(message);
		message = dao.findById(1);
		assertNotNull(message);
		assertEquals("xxxx", message.getMessageText());
		assertEquals(2, message.getRecipients().size());
	}

	@Test
	public void testFindMessagesToBeSent() throws PersistenceException {
		Collection<SystemMessage> coll = dao.findMessagesToBeSent(0, 5);
		assertEquals(1, coll.size());
		coll = dao.findMessagesToBeSent(1, 5);
		assertEquals(2, coll.size());
		coll = dao.findMessagesToBeSent(2, 5);
		assertEquals(0, coll.size());
		coll = dao.findMessagesToBeSent(3, 5);
		assertEquals(0, coll.size());

		// Update an already existing message
		SystemMessage message = dao.findById(1);
		assertNotNull(message);
		assertEquals("message text1", message.getMessageText());
		message.setTrials(5);
		dao.store(message);
		message = dao.findById(1);
		assertNotNull(message);
		coll = dao.findMessagesToBeSent(1, 5);
		assertEquals(1, coll.size());

		message = dao.findById(2);
		assertNotNull(message);
		assertEquals("message text2", message.getMessageText());
		message.setType(0);
		dao.store(message);
		message = dao.findById(2);
		assertNotNull(message);
		coll = dao.findMessagesToBeSent(0, 5);
		assertEquals(2, coll.size());
	}
}
