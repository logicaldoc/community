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
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * Hibernate implementation of <code>RatingDAO</code>
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.1
 */
@SuppressWarnings("unchecked")
public class HibernateRatingDAO extends HibernatePersistentObjectDAO<Rating> implements RatingDAO {

	public HibernateRatingDAO() {
		super(Rating.class);
		super.log = LoggerFactory.getLogger(HibernateRatingDAO.class);
	}

	@Override
	public boolean store(Rating rating, History transaction) {
		boolean result = super.store(rating);
		if (!result)
			return false;

		try {
			if (transaction != null) {
				transaction.setEvent(DocumentEvent.RATING_NEW.toString());
				transaction.setComment("rating: " + rating.getVote());
				
				DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				documentDao.saveDocumentHistory(documentDao.findById(rating.getDocId()), transaction);
			}
		} catch (Throwable e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public Rating findVotesByDocId(long docId) {
		List<Rating> coll = new ArrayList<Rating>();
		try {

			/*
			 * Don't use AVG function to have more control on rounding policy
			 */
			String query = "select count(*), SUM(ld_vote) from ld_rating where ld_deleted=0 and ld_docid = " + docId;

			@SuppressWarnings("rawtypes")
			RowMapper ratingMapper = new BeanPropertyRowMapper() {
				public Object mapRow(ResultSet rs, int rowNum) throws SQLException {

					Rating rating = new Rating();
					rating.setCount(rs.getInt(1));
					if (rs.getInt(1) > 0) {
						float div = (float) rs.getInt(2) / (float) rs.getInt(1);
						double avg = Math.round(div * 100.0) / 100.0;
						rating.setAverage(new Double(avg).floatValue());
					} else
						rating.setAverage(0F);

					return rating;
				}
			};

			coll = (List<Rating>) query(query, new Object[] {}, ratingMapper, null);
			if (!coll.isEmpty() && coll.get(0).getCount() != 0)
				return coll.get(0);

		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
		}
		return null;
	}

	@Override
	public Rating findByDocIdAndUserId(long docId, long userId) {
		List<Rating> results = findByWhere("_entity.docId =" + docId + " and _entity.userId =" + userId, null, null);
		if (results.isEmpty())
			return null;
		else
			return results.get(0);
	}

	@Override
	public List<Rating> findByDocId(long docId) {
		return findByWhere("_entity.docId = " + docId, " order by _entity.lastModified desc", null);
	}

	@Override
	public boolean delete(long id, int code) {
		jdbcUpdate("delete from ld_rating where ld_id=" + id);
		return true;
	}
}