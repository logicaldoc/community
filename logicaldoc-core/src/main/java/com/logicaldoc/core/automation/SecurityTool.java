package com.logicaldoc.core.automation;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;

/**
 * Utility methods to handle some security related operations from within the
 * Automation
 * 
 * @author Meschieri - LogicalDOC
 * 
 * @since 8.4
 */
@AutomationDictionary
public class SecurityTool {

	private static final Logger log = LoggerFactory.getLogger(SecurityTool.class);

	/**
	 * Retrieves a user object
	 * 
	 * @param username the username
	 * 
	 * @return the user object
	 */
	public User getUser(String username) {
		UserDAO userDao = Context.get(UserDAO.class);
		try {
			return StringUtils.isNotEmpty(username) ? userDao.findByUsername(username)
					: userDao.findByUsername("_system");
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Retrieves a user object
	 * 
	 * @param userId the user ID
	 * 
	 * @return the user object
	 */
	public User getUser(long userId) {
		UserDAO userDao = Context.get(UserDAO.class);
		try {
			return userDao.findById(userId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Retrieves a session by it's identifier
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return the session that matches the identifier
	 */
	public Session getSession(String sid) {
		return SessionManager.get().get(sid);
	}
}