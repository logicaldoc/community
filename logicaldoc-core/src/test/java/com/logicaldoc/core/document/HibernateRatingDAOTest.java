package com.logicaldoc.core.document;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.RatingDAO;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateRatingDAOTest</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class HibernateRatingDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private RatingDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateRatingDAO
		dao = (RatingDAO) context.getBean("RatingDAO");
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
	}

	@Test
	public void testFindVotesByDocId() {
		Rating rat1 = dao.findVotesByDocId(1);
		Assert.assertNotNull(rat1);
		Assert.assertEquals(2, rat1.getCount().intValue());
		Assert.assertEquals(new Float(2.5), rat1.getAverage().floatValue());

		rat1 = dao.findVotesByDocId(2);
		Assert.assertNotNull(rat1);
		Assert.assertEquals(1, rat1.getCount().intValue());
		Assert.assertEquals(new Float(3.0), rat1.getAverage().floatValue());

		// Try with unexisting rating vote
		rat1 = dao.findVotesByDocId(99);
		Assert.assertNull(rat1);
	}

	@Test
	public void testFindByDocId() {
		List<Rating> ratings = dao.findByDocId(1L);
		Assert.assertEquals(2, ratings.size());
		
		// Try with unexisting rating vote
		ratings = dao.findByDocId(99L);
		Assert.assertTrue(ratings.isEmpty());
	}

	@Test
	public void testFindByDocIdAndUserId() {
		Assert.assertNotNull(dao.findByDocIdAndUserId(1, 1));
		Assert.assertNull(dao.findByDocIdAndUserId(2, 2));
	}
}
