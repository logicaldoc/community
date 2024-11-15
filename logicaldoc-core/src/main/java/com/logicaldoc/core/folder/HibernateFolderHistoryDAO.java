package com.logicaldoc.core.folder;

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
 * Hibernate implementation of <code>FolderHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class HibernateFolderHistoryDAO extends HibernateHistoryDAO<FolderHistory> implements FolderHistoryDAO {

	private static final String AND = " and ";

	private static final String DATE_ASC = ".date asc";

	private static final String ORDER_BY = "order by ";

	private HibernateFolderHistoryDAO() {
		super(FolderHistory.class);
		super.log = LoggerFactory.getLogger(HibernateFolderHistoryDAO.class);
	}

	@Override
	public List<FolderHistory> findByUserId(long userId) throws PersistenceException {
		return findByUserIdAndEvent(userId, null);
	}

	@Override
	public List<FolderHistory> findByFolderId(long folderId) throws PersistenceException {
		return findByWhere(ENTITY + ".folderId =" + folderId, ORDER_BY + ENTITY + DATE_ASC, null);
	}

	@Override
	public List<FolderHistory> findNotNotified(Integer max) throws PersistenceException {
		return findByWhere(ENTITY + ".notified = 0", ORDER_BY + ENTITY + DATE_ASC, max);
	}

	@Override
	public void cleanOldHistories(int ttl) throws PersistenceException {
		log.info("cleanOldHistories rows updated: {}", cleanOldRecords(ttl, "ld_folder_history"));
	}

	@Override
	public List<FolderHistory> findByUserIdAndEvent(long userId, String event) throws PersistenceException {
		String query = ENTITY + ".userId =" + userId;
		if (event != null && StringUtils.isNotEmpty(event))
			query += " and lower(" + ENTITY + ".event) like '" + SqlUtil.doubleQuotes(event.toLowerCase()) + "'";

		return findByWhere(query, ORDER_BY + ENTITY + DATE_ASC, null);
	}

	@Override
	public List<FolderHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
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

	@Override
	public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate)
			throws PersistenceException {
		String query = ENTITY + ".folderId = :folderId and " + ENTITY + ".event = :event ";

		Map<String, Object> params = new HashMap<>();
		params.put("folderId", folderId);
		params.put("event", event);

		if (oldestDate != null) {
			query += AND + ENTITY + ".date >= :oldestDate ";
			params.put("oldestDate", oldestDate);
		}

		return findByWhere(query, params, ORDER_BY + ENTITY + DATE_ASC, null);
	}
}