package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Calendar;
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
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserHistory;

public class HibernateUserHistoryDAO extends HibernatePersistentObjectDAO<UserHistory> implements UserHistoryDAO {

	private HibernateUserHistoryDAO() {
		super(UserHistory.class);
		super.log = LoggerFactory.getLogger(HibernateUserHistoryDAO.class);
	}

	@Override
	public List<UserHistory> findByUserId(long userId) {
		return findByUserIdAndEvent(userId, null);
	}

	public List<UserHistory> findByUserIdAndEvent(long userId, String event) {
		try {
			if (StringUtils.isEmpty(event))
				return findByWhere("_entity.userId =" + userId, "order by _entity.date desc", null);
			else {
				Map<String, Object> params = new HashMap<String, Object>();
				params.put("userId", userId);
				params.put("event", event);

				return findByWhere("_entity.userId = :userId and _entity.event = :event", params,
						"order by _entity.date desc", null);
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<UserHistory>();
		}
	}

	@Override
	public UserHistory createUserHistory(User user, String eventType, String comment, String sessionId, Client client) {
		UserHistory history = new UserHistory();
		history.setComment(comment);
		history.setEvent(eventType);

		Session session = SessionManager.get().get(sessionId);
		if (session != null)
			history.setSession(session);
		else
			history.setSessionId(sessionId);

		if (user != null)
			history.setUser(user);

		if (client != null) {
			history.setIp(client.getAddress());
			if (client.getDevice() != null)
				history.setDevice(client.getDevice().toString());
			if (client.getGeolocation() != null)
				history.setGeolocation(client.getGeolocation().toString());
		}

		try {
			store(history);
			return history;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * @see com.logicaldoc.core.security.dao.UserHistoryDAO#cleanOldHistories(int)
	 */
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
				int rowsUpdated = jdbcUpdate("UPDATE ld_user_history SET ld_deleted = 1, ld_lastmodified = ?"
						+ " WHERE ld_deleted = 0 AND ld_date < ?", today, ldDate);
				log.info("cleanOldHistories rows updated: {}", rowsUpdated);
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}

		}
	}

	@Override
	public boolean store(UserHistory history) throws PersistenceException {
		// Write only if the history is enabled
		if (RunLevel.current().aspectEnabled(DocumentHistory.ASPECT)) {
			if (history.getComment() != null && history.getComment().length() > 4000)
				history.setComment(StringUtils.abbreviate(history.getComment(), 4000));
			boolean ret = super.store(history);
			if (ret)
				EventCollector.get().newEvent(history);
			return ret;
		} else
			return true;
	}
}