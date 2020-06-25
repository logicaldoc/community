package com.logicaldoc.web.util;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;

import com.logicaldoc.core.PersistenceException;
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
import com.logicaldoc.i18n.I18N;
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
	 * @param sid identifier of the session
	 * 
	 * @return the session
	 * 
	 * @throws InvalidSessionException the session does not exist or is expired
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
	 * Checks if a specific menu is accessible by the user in the current
	 * session
	 * 
	 * @param request the HTTP request
	 * @param menuIds identifiers of the menus
	 * 
	 * @return the current session
	 * 
	 * @throws InvalidSessionException the session does not exist or is expired
	 * @throws AccessDeniedException the user cannot access any menu
	 */
	public static Session checkEvenOneMenu(HttpServletRequest request, long... menuIds)
			throws InvalidSessionException, AccessDeniedException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		for (long menuId : menuIds) {
			if (dao.isReadEnable(menuId, session.getUserId()))
				return session;
		}

		String message = String.format("User %s cannot access the menus %s", session.getUsername(),
				Arrays.asList(menuIds));
		throw new AccessDeniedException(message);
	}

	/**
	 * Check if a specific menu is accessible by the user in the current session
	 * 
	 * @param request the HTTP request
	 * @param menuId identifier of the menus
	 * 
	 * @return the curent session
	 * 
	 * @throws InvalidSessionException the session does not exist or is expired
	 * @throws AccessDeniedException the user cannot access any menu
	 */
	public static Session checkMenu(HttpServletRequest request, long menuId)
			throws InvalidSessionException, AccessDeniedException {
		Session session = validateSession(request);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!dao.isReadEnable(menuId, session.getUserId())) {
			String message = String.format("User %s cannot access the menu %s", session.getUsername(), menuId);
			throw new AccessDeniedException(message);
		}
		return session;
	}

	public static void checkPermission(Permission permission, User user, long folderId) throws AccessDeniedException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(permission, folderId, user.getId())) {
			String message = String.format("User %s doesn't have permission %s on folder %s", user.getUsername(),
					permission.getName(), folderId);
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

		if (message != null)
			message = message.replaceAll("com.logicaldoc.", "").replaceAll("java.lang.", "");

		if (t != null && (t instanceof org.hibernate.TransactionException
				|| t instanceof org.hibernate.HibernateException || t instanceof PersistenceException
				|| t instanceof org.springframework.transaction.TransactionSystemException)) {
			message = I18N.message("dberrorretry", session.getUser().getLocale());
		}

		throw new ServerException(message);
	}

	/**
	 * To always deal with dates and not Timestamps
	 * 
	 * @param src the source date
	 * 
	 * @return if <code>src</code> is instance of {@link Timestamp} it will be
	 *         converted to plain {@link Date}
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