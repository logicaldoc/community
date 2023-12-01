package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.History;
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

	private static final String AND = " and ";

	private static final String DATE_ASC = ".date asc";

	private static final String ORDER_BY = "order by ";

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
			return findByWhere(ENTITY + ".folderId =" + folderId, ORDER_BY + ENTITY + DATE_ASC, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<FolderHistory> findNotNotified(Integer max) {
		try {
			return findByWhere(ENTITY + ".notified = 0", ORDER_BY + ENTITY + DATE_ASC, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void cleanOldHistories(int ttl) {
		try {
			log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_folder_history"));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public List<FolderHistory> findByUserIdAndEvent(long userId, String event) {
		String query = ENTITY + ".userId =" + userId;
		if (event != null && StringUtils.isNotEmpty(event))
			query += " and lower(" + ENTITY + ".event) like '" + SqlUtil.doubleQuotes(event.toLowerCase()) + "'";

		try {
			return findByWhere(query, ORDER_BY + ENTITY + DATE_ASC, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public void store(FolderHistory history) throws PersistenceException {
		// Write only if the history is enabled
		if (RunLevel.current().aspectEnabled(History.ASPECT)) {
			if (history.getDate() == null)
				history.setDate(new Date());
			if (history.getComment() != null && history.getComment().length() > 4000)
				history.setComment(StringUtils.abbreviate(history.getComment(), 4000));
			super.store(history);
			EventCollector.get().newEvent(history);
		}
	}

	@Override
	public List<FolderHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
			Integer max) {
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

		try {
			return findByWhere(query.toString(), params, ORDER_BY + ENTITY + DATE_ASC, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate) {
		String query = ENTITY + ".folderId = :folderId and " + ENTITY + ".event = :event ";

		Map<String, Object> params = new HashMap<>();
		params.put("folderId", folderId);
		params.put("event", event);

		if (oldestDate != null) {
			query += AND + ENTITY + ".date >= :oldestDate ";
			params.put("oldestDate", oldestDate);
		}

		try {
			return findByWhere(query, params, ORDER_BY + ENTITY + DATE_ASC, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}
}