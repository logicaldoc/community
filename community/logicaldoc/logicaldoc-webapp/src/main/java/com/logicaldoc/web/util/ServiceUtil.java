package com.logicaldoc.web.util;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;

import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Session.Log;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.util.Context;

/**
 * Various methods related to the user session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ServiceUtil {
	public static final String LOCALE = "locale";

	public static final String USER = "user";

	public static Session validateSession(HttpServletRequest request) throws InvalidSessionException {
		String sid = SessionManager.get().getSessionId(request);
		return validateSession(sid);
	}

	/**
	 * Throws a runtime exception id the given session is invalid
	 * 
	 * @throws InvalidSessionException
	 */
	public static Session validateSession(String sid) throws InvalidSessionException {
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new InvalidSessionException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new InvalidSessionException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}

	/**
	 * Checks if a specific menu is accessible by the user in the current session
	 */
	public static Session checkEvenOneMenu(HttpServletRequest request, long... menuIds) throws InvalidSessionException,
			AccessDeniedException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		for (long menuId : menuIds) {
			if (dao.isReadEnable(menuId, session.getUserId()))
				return session;
		}

		String message = "User " + session.getUsername() + " cannot access the menues " + Arrays.asList(menuIds);
		throw new AccessDeniedException(message);
	}

	/**
	 * Check if a specific menu is accessible by the user in the current session
	 */
	public static Session checkMenu(HttpServletRequest request, long menuId) throws InvalidSessionException,
			AccessDeniedException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!dao.isReadEnable(menuId, session.getUserId())) {
			String message = "User " + session.getUsername() + " cannot access the menu " + menuId;
			throw new AccessDeniedException(message);
		}
		return session;
	}

	public static void checkPermission(Permission permission, User user, long folderId) throws AccessDeniedException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(permission, folderId, user.getId())) {
			String message = "User " + user.getUsername() + " doesn't have permission " + permission.getName()
					+ " on folder " + folderId;
			throw new AccessDeniedException(message);
		}
	}

	public static Locale currentLocale(Session session) throws InvalidSessionException {
		return (Locale) session.getDictionary().get(LOCALE);
	}

	public static Locale currentLocale(String sid) throws InvalidSessionException {
		Session session = validateSession(sid);
		return currentLocale(session);
	}

	public static User getSessionUser(String sid) throws InvalidSessionException {
		Session session = validateSession(sid);
		User user = (User) session.getDictionary().get(USER);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);
		return user;
	}

	public static User getSessionUser(HttpServletRequest request) throws InvalidSessionException {
		Session session = validateSession(request);
		User user = (User) session.getDictionary().get(USER);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);
		return user;
	}

	public static Object throwServerException(Session session, Logger logger, Throwable t) throws ServerException {
		if (logger != null)
			logger.error(t.getMessage(), t);

		String message = t.getMessage();
		if (session != null) {
			Log lastError = session.getLastError();
			if (lastError != null) {
				message = lastError.getMessage();
				session.getLogs().clear();
			}
		}

		message = message.replaceAll("com.logicaldoc.", "").replaceAll("java.lang.", "");
		throw new ServerException(message);
	}

	/**
	 * To always deal with dates and not Timestamps
	 */
	public static Date convertToDate(Date src) {
		if (src == null)
			return null;

		if (src instanceof Timestamp) {
			Calendar cal = Calendar.getInstance();
			cal.setTimeInMillis(src.getTime());
			return cal.getTime();
		} else {
			return src;
		}
	}
}