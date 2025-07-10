package com.logicaldoc.core.security.user;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;

import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>PasswordHistoryDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
@Repository("passwordHistoryDAO")
@Transactional
public class HibernatePasswordHistoryDAO extends HibernatePersistentObjectDAO<PasswordHistory>
		implements PasswordHistoryDAO {

	private HibernatePasswordHistoryDAO() {
		super(PasswordHistory.class);
		super.log = LoggerFactory.getLogger(PasswordHistoryDAO.class);
	}

	@Override
	public List<PasswordHistory> findByUserId(long userId, Integer max) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("userId", userId);
		return findByWhere(ENTITY + ".userId = :userId", params, ENTITY + ".date desc", max);
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
		} catch (Exception t) {
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
				if (history.getPassword() != null && history.getPassword().equals(password))
					return history;
			}
		}
		return null;
	}
}