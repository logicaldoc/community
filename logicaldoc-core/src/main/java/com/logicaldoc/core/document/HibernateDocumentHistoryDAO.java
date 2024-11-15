package com.logicaldoc.core.document;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernateHistoryDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>HistoryDAO</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentHistoryDAO extends HibernateHistoryDAO<DocumentHistory> implements DocumentHistoryDAO {

	private static final String DATE_ASC = ".date asc";

	private static final String ORDER_BY = "order by ";

	private static final String AND = " and ";

	private HibernateDocumentHistoryDAO() {
		super(DocumentHistory.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentHistoryDAO.class);
	}

	@Override
	public List<DocumentHistory> findByDocId(long docId) throws PersistenceException {
		return findByDocIdAndEvent(docId, null);
	}

	@Override
	public List<DocumentHistory> findByDocIdAndEvent(long docId, String event) throws PersistenceException {
		StringBuilder query = new StringBuilder(" " + ENTITY + ".docId = " + docId);
		if (StringUtils.isNotEmpty(event))
			query.append(AND + ENTITY + ".event='" + SqlUtil.doubleQuotes(event) + "'");
		return findByWhere(query.toString(), ORDER_BY + ENTITY + ".date desc", null);
	}

	@Override
	public List<DocumentHistory> findByUserId(long userId) throws PersistenceException {
		return findByUserIdAndEvent(userId, null, null);
	}

	@Override
	public List<DocumentHistory> findByFolderId(long folderId) throws PersistenceException {
		return findByWhere(ENTITY + ".folderId =" + folderId, ORDER_BY + ENTITY + DATE_ASC, null);
	}

	@Override
	public List<DocumentHistory> findNotNotified(Integer max) throws PersistenceException {
		return findByWhere(ENTITY + ".notified = 0", ORDER_BY + ENTITY + DATE_ASC, max);
	}

	@Override
	public void cleanOldHistories(int ttl) throws PersistenceException {
		log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_history"));
	}

	@Override
	public List<DocumentHistory> findByUserIdAndEvent(long userId, String event, String sessionId)
			throws PersistenceException {
		String query = ENTITY + ".userId =" + userId;
		if (event != null && StringUtils.isNotEmpty(event))
			query += AND + ENTITY + ".event = '" + SqlUtil.doubleQuotes(event) + "'";
		if (sessionId != null && StringUtils.isNotEmpty(sessionId))
			query += AND + ENTITY + ".sessionId = '" + sessionId + "'";

		return findByWhere(query, ORDER_BY + ENTITY + DATE_ASC, null);
	}

	@Override
	public void markHistoriesAsRead(String event, long userId) throws PersistenceException {
		String statement = "update ld_history set ld_new=0 where ld_new=1 and ld_userid=" + userId + " and ld_event='"
				+ SqlUtil.doubleQuotes(event) + "'";
		jdbcUpdate(statement);
	}

	@Override
	public List<DocumentHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
			Integer max) throws PersistenceException {
		StringBuilder query = new StringBuilder(
				"(" + ENTITY + ".path like :pathExpression or " + ENTITY + ".pathOld like :pathExpression) ");
		Map<String, Object> params = new HashMap<>();
		params.put("pathExpression", pathExpression);

		if (oldestDate != null) {
			query.append(AND + ENTITY + ".date >= :oldestDate ");
			params.put("oldestDate", oldestDate);
		}

		if (events != null && !events.isEmpty()) {
			StringBuilder eventsStr = new StringBuilder("(");
			for (String event : events) {
				if (eventsStr.length() > 1)
					eventsStr.append(",");
				eventsStr.append("'" + event + "'");
			}
			eventsStr.append(")");
			query.append(AND + ENTITY + ".event in " + eventsStr);
		}

		return findByWhere(query.toString(), params, ORDER_BY + ENTITY + DATE_ASC, max);
	}
}