package com.logicaldoc.core.automation;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
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

	protected static Logger log = LoggerFactory.getLogger(SecurityTool.class);

	/**
	 * Retrieves a user object
	 * 
	 * @param username the username
	 * 
	 * @return the user object
	 */
	public User getUser(String username) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = StringUtils.isNotEmpty(username) ? userDao.findByUsername(username)
				: userDao.findByUsername("_system");
		return user;
	}

	/**
	 * Retrieves a user object
	 * 
	 * @param userId the user ID
	 * 
	 * @return the user object
	 */
	public User getUser(long userId) {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		return userDao.findById(userId);
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