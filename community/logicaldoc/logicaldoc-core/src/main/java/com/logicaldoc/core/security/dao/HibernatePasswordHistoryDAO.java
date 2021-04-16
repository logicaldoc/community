package com.logicaldoc.core.security.dao;

import java.util.List;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.PasswordHistory;

/**
 * Hibernate implementation of <code>PasswordHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class HibernatePasswordHistoryDAO extends HibernatePersistentObjectDAO<PasswordHistory>
		implements PasswordHistoryDAO {

	private HibernatePasswordHistoryDAO() {
		super(PasswordHistory.class);
		super.log = LoggerFactory.getLogger(PasswordHistoryDAO.class);
	}

	@Override
	public List<PasswordHistory> findByUserId(long userId, Integer max) throws PersistenceException {
		return findByWhere("_entity.userId = ?1", new Object[] { userId }, "_entity.date desc", max);
	}

	@Override
	public void cleanOldHistories(long userId, int retain) {
		if (retain <= 0)
			return;
		try {
			List<PasswordHistory> histories = findByUserId(userId, null);
			if (histories.size() <= retain)
				return;
			for (int i = 0; i < histories.size(); i++) {
				if (i >= retain)
					delete(histories.get(i).getId());
			}
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		}

	}

	@Override
	public PasswordHistory findByUserIdAndPassword(long userId, String password, int max) throws PersistenceException {
		if (password != null) {
			List<PasswordHistory> histories = findByUserId(userId, max);
			for (int i = 0; i < histories.size(); i++) {
				if (i >= max)
					return null;
				PasswordHistory history = histories.get(i);
				if (history.getPassword().equals(password))
					return history;
			}
		}
		return null;
	}
}