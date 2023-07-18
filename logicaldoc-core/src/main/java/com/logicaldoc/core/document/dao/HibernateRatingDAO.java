package com.logicaldoc.core.document.dao;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * Hibernate implementation of <code>RatingDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
@SuppressWarnings("unchecked")
public class HibernateRatingDAO extends HibernatePersistentObjectDAO<Rating> implements RatingDAO {

	public HibernateRatingDAO() {
		super(Rating.class);
		super.log = LoggerFactory.getLogger(HibernateRatingDAO.class);
	}

	@Override
	public void store(Rating rating, DocumentHistory transaction) throws PersistenceException {
		super.store(rating);

		flush();

		try {
			if (transaction != null) {
				transaction.setEvent(DocumentEvent.RATING_NEW.toString());
				transaction.setComment("rating: " + rating.getVote());
			}
			updateDocumentRating(rating.getDocId(), transaction);
		} catch (Exception e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public int updateDocumentRating(long docId, DocumentHistory transaction) throws PersistenceException {
		Rating votesDoc = findVotesByDocId(docId);
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document doc = docDao.findById(docId);
		if (doc == null)
			return 0;

		docDao.initialize(doc);
		int average = 0;
		if (votesDoc != null && votesDoc.getAverage() != null)
			average = votesDoc.getAverage().intValue();
		doc.setRating(average);
		docDao.store(doc, transaction);
		return average;
	}

	@Override
	public Rating findVotesByDocId(long docId) {
		List<Rating> coll = new ArrayList<>();
		try {

			/*
			 * Don't use AVG function to have more control on rounding policy
			 */
			String query = "select count(*), SUM(ld_vote) from ld_rating where ld_deleted=0 and ld_docid = " + docId;

			@SuppressWarnings("rawtypes")
			RowMapper ratingMapper = new BeanPropertyRowMapper() {
				@Override
				public Object mapRow(ResultSet rs, int rowNum) throws SQLException {

					Rating rating = new Rating();
					rating.setCount(rs.getInt(1));
					if (rs.getInt(1) > 0) {
						float div = (float) rs.getInt(2) / (float) rs.getInt(1);
						double avg = Math.round(div * 100.0) / 100.0;
						rating.setAverage((float) avg);
					} else
						rating.setAverage(0F);

					return rating;
				}
			};

			coll = query(query, ratingMapper, null);
			if (!coll.isEmpty() && coll.get(0).getCount() != 0)
				return coll.get(0);

		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
		}
		return null;
	}

	@Override
	public Rating findByDocIdAndUserId(long docId, long userId) {
		List<Rating> results = new ArrayList<>();
		try {
			results = findByWhere(ENTITY + ".docId =" + docId + " and " + ENTITY + ".userId =" + userId,
					null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (results.isEmpty())
			return null;
		else
			return results.get(0);
	}

	@Override
	public List<Rating> findByDocId(long docId) {
		try {
			return findByWhere(ENTITY + ".docId = " + docId, " order by " + ENTITY + ".lastModified desc",
					null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		Rating rat = findById(id);
		long docId = rat.getDocId();

		jdbcUpdate("delete from ld_rating where ld_id=" + id);

		updateDocumentRating(docId, null);
	}
}