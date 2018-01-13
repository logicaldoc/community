package com.logicaldoc.core.communication;

import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;

/**
 * Test case for <code>HibernateSystemMessageDAO</code>
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 3.0
 */
public class HibernateSystemMessageDAOTest extends AbstractCoreTCase {
	// Instance under test
	private SystemMessageDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context.
		// Make sure that it is an HibernateSystemMessageDAO
		dao = (SystemMessageDAO) context.getBean("SystemMessageDAO");
	}

	@Test
	public void testDelete() {
		Assert.assertTrue(dao.delete(1));
		SystemMessage message = dao.findById(1);
		Assert.assertNull(message);
	}

	@Test
	public void testFindById() {
		SystemMessage message = dao.findById(1);
		Assert.assertNotNull(message);
		Assert.assertEquals(1, message.getId());
		Assert.assertEquals("message text1", message.getMessageText());

		// Try with unexisting message
		message = dao.findById(99);
		Assert.assertNull(message);
	}

	@Test
	public void testFindByRecipient() {
		Collection<SystemMessage> coll = dao.findByRecipient("sebastian", SystemMessage.TYPE_SYSTEM, null);
		Assert.assertEquals(1, coll.size());
		coll = dao.findByRecipient("marco", 1, null);
		Assert.assertEquals(2, coll.size());
		coll = dao.findByRecipient("paperino", 2, null);
		Assert.assertEquals(0, coll.size());
		coll = dao.findByRecipient("xxxx", SystemMessage.TYPE_SYSTEM, null);
		Assert.assertEquals(0, coll.size());
	}

	@Test
	public void testDeleteExpiredMessages() {
		dao.deleteExpiredMessages("sebastian");
		Assert.assertNotNull(dao.findById(1));

		dao.deleteExpiredMessages(SystemMessage.TYPE_SYSTEM);
		Assert.assertNotNull(dao.findById(1));
	}

	@Test
	public void testGetCount() {
		Assert.assertEquals(1, dao.getCount("sebastian", SystemMessage.TYPE_SYSTEM, null));
		Assert.assertEquals(2, dao.getCount("marco", 1, null));
		Assert.assertEquals(0, dao.getCount("admin", SystemMessage.TYPE_SYSTEM, null));
	}

	@Test
	public void testFindByType() {
		Collection<SystemMessage> coll = dao.findByType(0);
		Assert.assertEquals(2, coll.size());
		coll = dao.findByType(1);
		Assert.assertEquals(2, coll.size());
		coll = dao.findByType(2);
		Assert.assertEquals(1, coll.size());
		coll = dao.findByType(3);
		Assert.assertEquals(0, coll.size());
	}

	@Test
	public void testFindByMode() {
		Collection<SystemMessage> coll = dao.findByMode("CC");
		Assert.assertEquals(1, coll.size());
		coll = dao.findByMode("sms");
		Assert.assertEquals(2, coll.size());
		coll = dao.findByMode("socket");
		Assert.assertEquals(0, coll.size());
		coll = dao.findByMode("xxxx");
		Assert.assertEquals(0, coll.size());
	}

	@Test
	public void testStore() {
		Set<Recipient> recipients = new HashSet<Recipient>();
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
		message.setType(SystemMessage.TYPE_SYSTEM);
		message.setStatus(SystemMessage.STATUS_NEW);
		message.setRecipients(recipients);
		Assert.assertTrue(dao.store(message));
		message = dao.findById(message.getId());
		Assert.assertNotNull(message);
		Assert.assertEquals(2, message.getRecipients().size());

		// Update an already existing message
		message = dao.findById(1);
		Assert.assertNotNull(message);
		Assert.assertEquals("message text1", message.getMessageText());
		message.setMessageText("xxxx");
		message.setRecipients(recipients);
		dao.store(message);
		message = dao.findById(1);
		Assert.assertNotNull(message);
		Assert.assertEquals("xxxx", message.getMessageText());
		Assert.assertEquals(2, message.getRecipients().size());
	}

	@Test
	public void testFindMessagesToBeSent() {
		Collection<SystemMessage> coll = dao.findMessagesToBeSent(0, 5);
		Assert.assertEquals(1, coll.size());
		coll = dao.findMessagesToBeSent(1, 5);
		Assert.assertEquals(2, coll.size());
		coll = dao.findMessagesToBeSent(2, 5);
		Assert.assertEquals(0, coll.size());
		coll = dao.findMessagesToBeSent(3, 5);
		Assert.assertEquals(0, coll.size());

		// Update an already existing message
		SystemMessage message = dao.findById(1);
		Assert.assertNotNull(message);
		Assert.assertEquals("message text1", message.getMessageText());
		message.setTrials(5);
		dao.store(message);
		message = dao.findById(1);
		Assert.assertNotNull(message);
		coll = dao.findMessagesToBeSent(1, 5);
		Assert.assertEquals(1, coll.size());

		message = dao.findById(2);
		Assert.assertNotNull(message);
		Assert.assertEquals("message text2", message.getMessageText());
		message.setType(0);
		dao.store(message);
		message = dao.findById(2);
		Assert.assertNotNull(message);
		coll = dao.findMessagesToBeSent(0, 5);
		Assert.assertEquals(2, coll.size());
	}
}
