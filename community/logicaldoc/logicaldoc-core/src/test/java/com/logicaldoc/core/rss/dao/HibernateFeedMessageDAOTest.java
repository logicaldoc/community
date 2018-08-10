package com.logicaldoc.core.rss.dao;

import java.util.Collection;
import java.util.List;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.rss.FeedMessage;

/**
 * Test case for <code>HibernateFeedMessageDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class HibernateFeedMessageDAOTest extends AbstractCoreTCase {

	private FeedMessageDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context.
		dao = (FeedMessageDAO) context.getBean("FeedMessageDAO");
	}

	@Test
	public void testDelete() {
		Assert.assertTrue(dao.delete(1));
		FeedMessage feedMessage = dao.findById(1);
		Assert.assertNull(feedMessage);
	}

	@Test
	public void testFindAll() {
		Collection<FeedMessage> feedMessages = dao.findAll();
		Assert.assertNotNull(feedMessages);
		Assert.assertEquals(3, feedMessages.size());
	}

	@Test
	public void testFindByGuid() {
		FeedMessage feedMessage = dao.findByGuid("feed2_guid");
		Assert.assertNotNull(feedMessage);
		Assert.assertEquals(2, feedMessage.getId());
		Assert.assertEquals("feed2", feedMessage.getTitle());

		feedMessage = dao.findByGuid("xxx");
		Assert.assertNull(feedMessage);
	}

	@Test
	public void testFindByTitle() {
		List<FeedMessage> messages = dao.findByTitle("eed");
		Assert.assertEquals(0, messages.size());

		messages = dao.findByTitle("%eed%");
		Assert.assertEquals(3, messages.size());
	}
	
	@Test
	public void testCheckNotRead() {
		Assert.assertTrue(dao.checkNotRead());
		Assert.assertTrue(dao.delete(3));
		Assert.assertFalse(dao.checkNotRead());
	}

	@Test
	public void testStore() {
		FeedMessage feedMessage = new FeedMessage();
		feedMessage.setTitle("feed4");
		feedMessage.setDescription("feed4_guid");
		Assert.assertTrue(dao.store(feedMessage));
		feedMessage = dao.findById(feedMessage.getId());
		Assert.assertEquals("feed4", feedMessage.getTitle());
		Assert.assertEquals("feed4_guid", feedMessage.getDescription());
	}

	@Test
	public void testDeleteOld() {
		List<FeedMessage> feedMessages = dao.findAll();
		Assert.assertEquals(3, feedMessages.size());
		System.err.println("feedMessages.size(): " +feedMessages.size());
		Assert.assertTrue(feedMessages.contains(dao.findById(3)));

		dao.deleteOld();

		feedMessages = dao.findAll();
		System.err.println("feedMessages.size(): " +feedMessages.size());
		Assert.assertEquals(0, feedMessages.size());
		Assert.assertFalse(feedMessages.contains(dao.findById(3)));
	}
}
