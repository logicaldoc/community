package com.logicaldoc.core.automation;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
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
	 * Generates and retrieves the avatar image for a given user
	 * 
	 * @param username username of the user
	 * 
	 * @return the avatar in Base64
	 */
	public String getAvatar(String username) {
		return UserUtil.getAvatarImage(username);
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
		String content = UserUtil.getAvatarImage(username);
		return "<img src='data:image/png;base64," + content + "' style='border: 0px height: " + size + "px; width: "
				+ size + "px; vertical-align:middle;' />";
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
		String content = UserUtil.getAvatarImage(""+userId);
		return "<img src='data:image/png;base64," + content + "' style='border: 0px height: " + size + "px; width: "
				+ size + "px; vertical-align:middle;' />";
	}
}