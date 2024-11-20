package com.logicaldoc.core.automation;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.util.Context;

/**
 * Utility methods to handle some user related operations from within the
 * Automation
 * 
 * @author Meschieri - LogicalDOC
 * 
 * @since 8.6.1
 */
@AutomationDictionary
public class UserTool {

	protected static Logger log = LoggerFactory.getLogger(UserTool.class);

	/**
	 * Retrieves a user object
	 * 
	 * @param username the username
	 * 
	 * @return the user object
	 */
	public User getUser(String username) {
		UserDAO userDao = Context.get().getBean(UserDAO.class);
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
		UserDAO userDao = Context.get().getBean(UserDAO.class);
		try {
			return userDao.findById(userId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param user the user to initialize
	 */
	public void initialize(User user) {
		UserDAO uDao = Context.get().getBean(UserDAO.class);
		try {
			uDao.initialize(user);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Generates and retrieves the avatar image for a given user
	 * 
	 * @param username username of the user
	 * 
	 * @return the avatar in Base64
	 */
	public String getAvatar(String username) {
		try {
			return UserUtil.getAvatarImage(username);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return "";
		}
	}

	/**
	 * Generates the HTML img tag for displaying an avatar
	 * 
	 * @param username username of the user
	 * @param size image size
	 * 
	 * @return the avatar in Base64
	 */
	public String getAvatarImg(String username, int size) {
		try {
			String content = UserUtil.getAvatarImage(username);
			return "<img src='data:image/png;base64," + content + "' style='border: 0px height: " + size + "px; width: "
					+ size + "px; vertical-align:middle;' />";
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return "";
		}
	}

	/**
	 * Generates the HTML img tag for displaying an avatar
	 * 
	 * @param userId identifier of the user
	 * @param size image size
	 * 
	 * @return the avatar in Base64
	 */
	public String getAvatarImg(long userId, int size) {
		try {
			String content = UserUtil.getAvatarImage("" + userId);
			return "<img src='data:image/png;base64," + content + "' style='border: 0px height: " + size + "px; width: "
					+ size + "px; vertical-align:middle;' />";
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return "";
		}
	}
}