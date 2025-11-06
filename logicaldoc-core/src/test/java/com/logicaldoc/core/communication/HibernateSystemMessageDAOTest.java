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
	private SystemMessageDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context.
		// Make sure that it is an HibernateSystemMessageDAO
		testSubject = SystemMessageDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		testSubject.delete(1);
		SystemMessage message = testSubject.findById(1);
		assertNull(message);
	}

	@Test
	public void testFindById() throws PersistenceException {
		SystemMessage message = testSubject.findById(1);
		assertNotNull(message);
		assertEquals(1, message.getId());
		assertEquals("message text1", message.getMessageText());

		// Try with unexisting message
		message = testSubject.findById(99);
		assertNull(message);
	}

	@Test
	public void testFindByRecipient() throws PersistenceException {
		Collection<SystemMessage> coll = testSubject.findByRecipient("sebastian", Message.TYPE_SYSTEM, null);
		assertEquals(1, coll.size());
		coll = testSubject.findByRecipient("marco", 1, null);
		assertEquals(2, coll.size());
		coll = testSubject.findByRecipient("paperino", 2, null);
		assertEquals(0, coll.size());
		coll = testSubject.findByRecipient("xxxx", Message.TYPE_SYSTEM, null);
		assertEquals(0, coll.size());
	}

	@Test
	public void testDeleteExpiredMessages() throws PersistenceException {
		testSubject.deleteExpiredMessages("sebastian");
		assertNotNull(testSubject.findById(1));

		testSubject.deleteExpiredMessages(Message.TYPE_SYSTEM);
		assertNotNull(testSubject.findById(1));
	}

	@Test
	public void testGetUnreadCount() throws PersistenceException {
		assertEquals(1, testSubject.getUnreadCount("sebastian", Message.TYPE_SYSTEM));
		assertEquals(2, testSubject.getUnreadCount("marco", 1));
		assertEquals(0, testSubject.getUnreadCount("admin", Message.TYPE_SYSTEM));
	}

	@Test
	public void testFindByType() throws PersistenceException {
		Collection<SystemMessage> coll = testSubject.findByType(0);
		assertEquals(2, coll.size());
		coll = testSubject.findByType(1);
		assertEquals(2, coll.size());
		coll = testSubject.findByType(2);
		assertEquals(1, coll.size());
		coll = testSubject.findByType(3);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindByMode() throws PersistenceException {
		Collection<SystemMessage> coll = testSubject.findByMode("CC");
		assertEquals(1, coll.size());
		coll = testSubject.findByMode("sms");
		assertEquals(2, coll.size());
		coll = testSubject.findByMode("socket");
		assertEquals(0, coll.size());
		coll = testSubject.findByMode("xxxx");
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
		testSubject.store(message);
		assertNotNull(message);
		
		message = testSubject.findById(message.getId());
		testSubject.initialize(message);
		assertNotNull(message);
		assertEquals(2, message.getRecipients().size());

		// Update an already existing message
		message = testSubject.findById(1);
		assertNotNull(message);
		assertEquals("message text1", message.getMessageText());
		message.setMessageText("xxxx");
		message.setRecipients(recipients);
		testSubject.store(message);
		
		message = testSubject.findById(1);
		testSubject.initialize(message);
		
		assertNotNull(message);
		assertEquals("xxxx", message.getMessageText());
		assertEquals(2, message.getRecipients().size());
	}

	@Test
	public void testFindMessagesToBeSent() throws PersistenceException {
		Collection<SystemMessage> coll = testSubject.findMessagesToBeSent(0, 5);
		assertEquals(1, coll.size());
		coll = testSubject.findMessagesToBeSent(1, 5);
		assertEquals(2, coll.size());
		coll = testSubject.findMessagesToBeSent(2, 5);
		assertEquals(0, coll.size());
		coll = testSubject.findMessagesToBeSent(3, 5);
		assertEquals(0, coll.size());

		// Update an already existing message
		SystemMessage message = testSubject.findById(1);
		assertNotNull(message);
		assertEquals("message text1", message.getMessageText());
		message.setTrials(5);
		testSubject.store(message);
		message = testSubject.findById(1);
		assertNotNull(message);
		coll = testSubject.findMessagesToBeSent(1, 5);
		assertEquals(1, coll.size());

		message = testSubject.findById(2);
		assertNotNull(message);
		assertEquals("message text2", message.getMessageText());
		message.setType(0);
		testSubject.store(message);
		message = testSubject.findById(2);
		assertNotNull(message);
		coll = testSubject.findMessagesToBeSent(0, 5);
		assertEquals(2, coll.size());
	}
}
