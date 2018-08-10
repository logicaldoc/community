package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>FolderHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@SuppressWarnings("unchecked")
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
		return findByWhere("_entity.folderId =" + folderId, "order by _entity.date asc", null);
	}

	@Override
	public List<FolderHistory> findNotNotified(Integer max) {
		return findByWhere("_entity.notified = 0", "order by _entity.date asc", max);
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

		return findByWhere(query, "order by _entity.date asc", null);
	}

	@Override
	public boolean store(FolderHistory entity) {
		// Write only if the history is enabled
		if (RunLevel.current().aspectEnabled(FolderHistory.ASPECT)) {
			boolean ret = super.store(entity);
			if (ret)
				EventCollector.get().newEvent(entity);
			return ret;
		} else
			return true;
	}

	@Override
	public List<FolderHistory> findByPath(String pathExpression, Date olderDate, Collection<String> events, Integer max) {
		StringBuffer query = new StringBuffer("(_entity.path like '" + pathExpression + "' or _entity.pathOld like '"
				+ pathExpression + "') ");
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

		return findByWhere(query.toString(), params.toArray(new Object[0]), "order by _entity.date asc", max);
	}

	@Override
	public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate) {
		String query = "_entity.folderId = ?1 and _entity.event = ?2 ";
		List<Object> values = new ArrayList<Object>();
		values.add(folderId);
		values.add(event);

		if (oldestDate != null) {
			query += " and _entity.date >= ?3 ";
			values.add(oldestDate);
		}

		return findByWhere(query, values.toArray(new Object[0]), "order by _entity.date asc", null);
	}
}