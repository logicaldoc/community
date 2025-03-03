package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;

import java.io.IOException;
import java.sql.Date;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for {@link HibernateRatingDAOTest}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class HibernateRatingDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private RatingDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateRatingDAO
		dao = Context.get(RatingDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		Rating rat1 = dao.findById(1);
		dao.initialize(rat1);
		rat1.setVote(4);
		rat1.setUserId(3);
		dao.store(rat1);
		Assert.assertNotNull(rat1);

		Rating rat2 = dao.findById(2);
		dao.initialize(rat2);
		rat2.setVote(2);
		rat2.setDocId(4);
		dao.store(rat2);
		Assert.assertNotNull(rat2);

		rat1 = dao.findById(1);
		Assert.assertEquals(4, rat1.getVote());
		Assert.assertEquals(3, rat1.getUserId());
		rat2 = dao.findById(2);
		Assert.assertEquals(2, rat2.getVote());
		Assert.assertEquals(4, rat2.getDocId());

		// testing overridden method with Rating and DocumentHistory parameters
		DocumentHistoryDAO historyDao = Context.get(DocumentHistoryDAO.class);

		DocumentHistory history = historyDao.findById(1L);
		historyDao.initialize(history);
		assertNotNull(history);

		dao.store(rat2, history);
	}

	@Test
	public void testFindVotesByDocId() throws PersistenceException {
		Rating rat1 = dao.findVotesByDocId(1);
		Assert.assertNotNull(rat1);
		Assert.assertEquals(2, rat1.getCount().intValue());
		Assert.assertEquals(Float.valueOf(2.5F), rat1.getAverage().floatValue());

		rat1 = dao.findVotesByDocId(2);
		Assert.assertNotNull(rat1);
		Assert.assertEquals(1, rat1.getCount().intValue());
		Assert.assertEquals(Float.valueOf(3.0F), rat1.getAverage().floatValue());

		// Try with non-existing rating vote
		rat1 = dao.findVotesByDocId(99);
		Assert.assertNull(rat1);
	}

	@Test
	public void testFindByDocId() throws PersistenceException {
		List<Rating> ratings = dao.findByDocId(1L);
		Assert.assertEquals(2, ratings.size());

		// Try with non-existing rating vote
		ratings = dao.findByDocId(99L);
		Assert.assertTrue(ratings.isEmpty());
	}

	@Test
	public void testFindByDocIdAndUserId() throws PersistenceException {
		Assert.assertNotNull(dao.findByDocIdAndUserId(1, 1));
		Assert.assertNull(dao.findByDocIdAndUserId(2, 2));
	}

	@Test
	public void testUpdateDocumentRating() throws PersistenceException {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
		Document doc = docDao.findById(1L);
		docDao.initialize(doc);
		assertNotNull(doc);

		DocumentHistoryDAO historyDao = Context.get(DocumentHistoryDAO.class);
		DocumentHistory history = historyDao.findById(1L);
		historyDao.initialize(history);
		assertNotNull(history);

		dao.updateDocumentRating(doc.getId(), history);
	}

	@Test
	public void testDelete() throws PersistenceException {
		Rating rat1 = new Rating();
		rat1.setDocId(4);
		rat1.setUserId(2);
		dao.store(rat1);
		assertNotNull(rat1);

		dao.delete(1, 1);
	}

	@Test
	public void testHashCode() throws PersistenceException {
		Rating rating1 = dao.findById(1);
		assertNotNull(rating1);

		Rating rating2 = dao.findById(2);
		assertNotNull(rating2);

		assertNotSame(rating1.hashCode(), rating2.hashCode());
	}

	@Test
	public void testEquals() throws PersistenceException {
		Rating rating1 = new Rating();
		rating1.setDocId(1);
		rating1.setCreation(new Date(2025 - 25 - 20));
		dao.store(rating1);
		assertNotNull(rating1);

		Rating rating2 = new Rating();
		rating2.setDocId(2);
		rating2.setCreation(new Date(2025 - 25 - 21));
		dao.store(rating2);
		assertNotNull(rating2);

		Rating rating3 = new Rating();
		rating3.setDocId(3);
		rating3.setCreation(new Date(2025 - 25 - 20));
		dao.store(rating3);
		assertNotNull(rating3);

		assertEquals(rating1, rating1);
		assertEquals(false, rating2.equals(rating1));
		assertEquals(false, rating1.equals(rating2));
		
		assertEquals(false, rating1.equals(new Object()));

		assertEquals(false, rating1.getCreation().equals(rating2.getCreation()));
		assertEquals(rating1.getCreation(), rating3.getCreation());

		rating2.setId(101);

		assertEquals(false, rating1.equals(rating2));

		rating2.setId(2);
		assertEquals(false, rating1.equals(rating2));
		
		rating1.setId(100);
	    rating1.setDocId(1);
	    rating1.setUserId(4);
	    rating1.setVote(5);
	    rating2.setId(100);
	    rating2.setDocId(1);
	    rating2.setUserId(2);
	    rating2.setVote(5);
	    assertEquals(false, rating1.equals(rating2));
	    
	    rating1.setUserId(2);
	    rating2.setVote(4);
	    assertEquals(false, rating1.equals(rating2));
	    rating2.setVote(5);

	    assertEquals(true, rating1.equals(rating2));
	}
}
