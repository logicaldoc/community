package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>FolderHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class HibernateFolderHistoryDAO extends HibernatePersistentObjectDAO<FolderHistory> implements FolderHistoryDAO {

	private HibernateFolderHistoryDAO() {
		super(FolderHistory.class);
		super.log = LoggerFactory.getLogger(HibernateFolderHistoryDAO.class);
	}

	@Override
	public List<FolderHistory> findByUserId(long userId) {
		return findByUserIdAndEvent(userId, null);
	}

	@Override
	public List<FolderHistory> findByFolderId(long folderId) {
		try {
			return findByWhere("_entity.folderId =" + folderId, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<FolderHistory>();
		}
	}

	@Override
	public List<FolderHistory> findNotNotified(Integer max) {
		try {
			return findByWhere("_entity.notified = 0", "order by _entity.date asc", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<FolderHistory>();
		}
	}

	@Override
	public void cleanOldHistories(int ttl) {
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			try {
				int rowsUpdated = jdbcUpdate("UPDATE ld_folder_history SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_date < ?", today, ldDate);

				log.info("cleanOldHistories rows updated: " + rowsUpdated);
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public List<FolderHistory> findByUserIdAndEvent(long userId, String event) {
		String query = "_entity.userId =" + userId;
		if (event != null && StringUtils.isNotEmpty(event))
			query += " and lower(_entity.event) like '" + SqlUtil.doubleQuotes(event.toLowerCase()) + "'";

		try {
			return findByWhere(query, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<FolderHistory>();
		}
	}

	@Override
	public boolean store(FolderHistory history) throws PersistenceException {
		// Write only if the history is enabled
		if (RunLevel.current().aspectEnabled(FolderHistory.ASPECT)) {
			if (history.getComment() != null && history.getComment().length() > 4000)
				history.setComment(StringUtils.abbreviate(history.getComment(), 4000));
			boolean ret = super.store(history);
			if (ret)
				EventCollector.get().newEvent(history);
			return ret;
		} else
			return true;
	}

	@Override
	public List<FolderHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
			Integer max) {
		StringBuffer query = new StringBuffer(
				"(_entity.path like :pathExpression or _entity.pathOld like :pathExpression) ");
		Map<String, Object> params = new HashMap<String, Object>();
		params.put("pathExpression", pathExpression);

		if (oldestDate != null) {
			query.append(" and _entity.date >= :oldestDate ");
			params.put("oldestDate", oldestDate);
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
			return findByWhere(query.toString(), params, "order by _entity.date asc", max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<FolderHistory>();
		}
	}

	@Override
	public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate) {
		String query = "_entity.folderId = :folderId and _entity.event = :event ";

		Map<String, Object> params = new HashMap<String, Object>();
		params.put("folderId", folderId);
		params.put("event", event);

		if (oldestDate != null) {
			query += " and _entity.date >= :oldestDate ";
			params.put("oldestDate", oldestDate);
		}

		try {
			return findByWhere(query, params, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<FolderHistory>();
		}
	}
}