package com.logicaldoc.core.document.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>HistoryDAO</code>
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentHistoryDAO extends HibernatePersistentObjectDAO<DocumentHistory>
		implements DocumentHistoryDAO {

	private HibernateDocumentHistoryDAO() {
		super(DocumentHistory.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentHistoryDAO.class);
	}

	/**
	 * @see com.logicaldoc.core.document.dao.DocumentHistoryDAO#findByDocId(long)
	 */
	public List<DocumentHistory> findByDocId(long docId) {
		return findByDocIdAndEvent(docId, null);
	}

	@Override
	public List<DocumentHistory> findByDocIdAndEvent(long docId, String event) {
		StringBuffer query = new StringBuffer(" _entity.docId = " + docId);
		if (StringUtils.isNotEmpty(event))
			query.append(" and _entity.event='" + SqlUtil.doubleQuotes(event) + "'");

		try {
			return findByWhere(query.toString(), "order by _entity.date desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentHistory>();
		}
	}

	/**
	 * @see com.logicaldoc.core.document.dao.DocumentHistoryDAO#findByUserId(long)
	 */
	public List<DocumentHistory> findByUserId(long userId) {
		return findByUserIdAndEvent(userId, null, null);
	}

	/**
	 * @see com.logicaldoc.core.document.dao.DocumentHistoryDAO#findByFolderId(long)
	 */
	public List<DocumentHistory> findByFolderId(long folderId) {
		try {
			return findByWhere("_entity.folderId =" + folderId, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentHistory>();
		}
	}

	@Override
	public List<DocumentHistory> findNotNotified(Integer max) {
		try {
			return findByWhere("_entity.notified = 0", "order by _entity.date asc", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentHistory>();
		}
	}
	
	@Override
	public void cleanOldHistories(int ttl) {
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			log.debug("today: {}", today);
			log.debug("ldDate: {}", ldDate);

			try {
				int rowsUpdated = jdbcUpdate("UPDATE ld_history SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_date < ?", today, ldDate);

				log.info("cleanOldHistories rows updated: {}", rowsUpdated);
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}

		}
	}

	@Override
	public List<DocumentHistory> findByUserIdAndEvent(long userId, String event, String sessionId) {
		String query = "_entity.userId =" + userId;
		if (event != null && StringUtils.isNotEmpty(event))
			query += " and _entity.event = '" + SqlUtil.doubleQuotes(event) + "'";
		if (sessionId != null && StringUtils.isNotEmpty(sessionId))
			query += " and _entity.sessionId = '" + sessionId + "'";

		try {
			return findByWhere(query, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentHistory>();
		}
	}

	@Override
	public void markHistoriesAsRead(String event, long userId) {
		String statement = "update ld_history set ld_new=0 where ld_new=1 and ld_userid=" + userId + " and ld_event='"
				+ SqlUtil.doubleQuotes(event) + "'";
		try {
			jdbcUpdate(statement);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public boolean store(DocumentHistory history) throws PersistenceException {
		// Write only if the history is enabled
		if (RunLevel.current().aspectEnabled(DocumentHistory.ASPECT)) {
			boolean ret = super.store(history);
			if (ret)
				EventCollector.get().newEvent(history);
			return ret;
		} else
			return true;
	}

	@Override
	public List<DocumentHistory> findByPath(String pathExpression, Date olderDate, Collection<String> events,
			Integer max) {
		StringBuffer query = new StringBuffer(
				"(_entity.path like '" + pathExpression + "' or _entity.pathOld like '" + pathExpression + "') ");
		List<Object> params = new ArrayList<Object>();
		if (olderDate != null) {
			query.append(" and _entity.date >= ?1 ");
			params.add(olderDate);
		}
		if (events != null && !events.isEmpty()) {
			StringBuffer eventsStr = new StringBuffer("(");
			for (String event : events) {
				if (eventsStr.length() > 1)
					eventsStr.append(",");
				eventsStr.append("'" + event + "'");
			}
			eventsStr.append(")");
			query.append(" and _entity.event in " + eventsStr);
		}

		try {
			return findByWhere(query.toString(), params.toArray(new Object[0]), "order by _entity.date asc", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<DocumentHistory>();
		}
	}
}