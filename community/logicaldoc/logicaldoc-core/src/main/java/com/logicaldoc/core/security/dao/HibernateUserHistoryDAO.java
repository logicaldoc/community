package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserHistory;

public class HibernateUserHistoryDAO extends HibernatePersistentObjectDAO<UserHistory> implements UserHistoryDAO {

	private HibernateUserHistoryDAO() {
		super(UserHistory.class);
		super.log = LoggerFactory.getLogger(HibernateUserHistoryDAO.class);
	}

	/**
	 * @see com.logicaldoc.core.security.dao.UserHistoryDAO#findByUserId(long)
	 */
	public List<UserHistory> findByUserId(long userId) {
		try {
			return findByWhere("_entity.userId =" + userId, "order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<UserHistory>();
		}
	}

	/**
	 * @see com.logicaldoc.core.security.dao.UserHistoryDAO#createUserHistory(User,
	 *      String, String, String, String)
	 */
	@Override
	public void createUserHistory(User user, String eventType, String comment, String ip, String sessionId) {
		UserHistory history = new UserHistory();
		history.setUser(user);
		history.setEvent(eventType);
		history.setComment(comment);
		history.setIp(comment);
		history.setSessionId(sessionId);
		try {
			store(history);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
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
			boolean ret = super.store(history);
			if (ret)
				EventCollector.get().newEvent(history);
			return ret;
		} else
			return true;
	}
}