package com.logicaldoc.web.util;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Session.Log;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.threading.NotifyingThread;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.websockets.WebsocketTool;

/**
 * Various methods related to the user session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ServiceUtil {
	private static Logger log = LoggerFactory.getLogger(LongRunningOperationCompleteListener.class);

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

	/**
	 * Executes a given operation and waits a given amount of time for it's
	 * completion, if it does not complete in time the thread is left running in
	 * background. Useful for releasing the User Interface and avoiding
	 * browser's timeout when you fear that the execution may take too much
	 * time.
	 * 
	 * @param name Name of the operation
	 * @param runnable The operation to execute
	 * @param session The current session
	 * 
	 * @return true if the runnable already completed successfully
	 * 
	 * @throws Throwable Whatever error that may occur
	 */
	public static boolean executeLongRunningOperation(String name, Runnable runnable, Session session)
			throws Throwable {
		ThreadPools pools = (ThreadPools) Context.get().getBean(ThreadPools.class);

		/*
		 * Build the notifying thread and schedule for immediate execution (1ms
		 * delay)
		 */
		NotifyingThread task = new NotifyingThread(runnable, name);
		pools.schedule(task, "LongRunningOperations", 1);

		// Wait up to 20 seconds for completion
		while (task.getElapsedTime() < 20000 && !task.isOver()) {
			try {
				Thread.sleep(2000);
			} catch (Throwable t) {

			}
		}

		if (task.isOver() && task.getError() != null) {
			// In case it already completed with error, re-throw the exception
			throw task.getError();
		} else if (!task.isOver()) {
			// Otherwise detach the current thread but add a listener to notify
			// the pending users
			log.warn("Operation {} invoked by user {} is taking too long and it will continue in background",
					task.getName(), session.getUsername());
			new WebsocketTool().showMessage(session,
					I18N.message("operationtakestoolongotoback", session.getUser().getLocale()), "warn");
			task.addListener(new LongRunningOperationCompleteListener(session.getUsername()));
		}

		return task.isOver();
	}
	
	/**
	 * Prepares the extended attributes of an extensible object
	 * 
	 * @param template The template to consider
	 * @param extensibleObject The GUI object to consider
	 * 
	 * @return The array of attributes
	 */
	public static GUIAttribute[] prepareGUIAttributes(Template template, ExtensibleObject extensibleObject) {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		tDao.initialize(template);

		List<GUIAttribute> attributes = new ArrayList<GUIAttribute>();
		if (template == null || template.getAttributes() == null || template.getAttributes().isEmpty())
			return new GUIAttribute[0];
		try {
			if (template != null) {
				for (String attrName : template.getAttributeNames()) {
					Attribute templateExtAttr = template.getAttributes().get(attrName);
					GUIAttribute att = new GUIAttribute();
					att.setName(attrName);
					att.setSetId(templateExtAttr.getSetId());
					att.setPosition(templateExtAttr.getPosition());
					att.setLabel(templateExtAttr.getLabel());
					att.setMandatory(templateExtAttr.getMandatory() == 1);
					att.setHidden(templateExtAttr.getHidden() == 1);
					att.setMultiple(templateExtAttr.getMultiple() == 1);
					att.setParent(templateExtAttr.getParent());
					att.setStringValues(templateExtAttr.getStringValues());
					att.setEditor(templateExtAttr.getEditor());
					att.setStringValue(templateExtAttr.getStringValue());
					att.setIntValue(templateExtAttr.getIntValue());
					att.setBooleanValue(templateExtAttr.getBooleanValue());
					att.setDoubleValue(templateExtAttr.getDoubleValue());
					att.setDateValue(templateExtAttr.getDateValue());
					att.setOptions(new String[] { templateExtAttr.getStringValue() });

					if (extensibleObject != null) {
						Attribute attribute = extensibleObject.getAttribute(attrName);
						if (attribute != null) {
							att.setStringValues(attribute.getStringValues());
							att.setStringValue(attribute.getStringValue());
							att.setIntValue(attribute.getIntValue());
							att.setBooleanValue(attribute.getBooleanValue());
							att.setDoubleValue(attribute.getDoubleValue());
							att.setDateValue(attribute.getDateValue());
						} else
							att.setValue(templateExtAttr.getValue());
					}

					// Normalize dates
					if (att.getValue() instanceof Date)
						att.setValue(ServiceUtil.convertToDate((Date) att.getValue()));

					att.setType(templateExtAttr.getType());
					attributes.add(att);

					if (att.isMultiple() && extensibleObject != null) {
						// Get the other values
						List<Attribute> values = extensibleObject.getValueAttributes(att.getName());
						if (values.size() > 1) {
							// Skip the parent attribute
							values.remove(0);

							// Create the GUI attributes for the values
							for (Attribute valAttribute : values) {
								GUIAttribute valAtt = (GUIAttribute) att.clone();
								valAtt.setName(valAttribute.getName());
								valAtt.setParent(att.getName());
								valAtt.setMultiple(false);
								valAtt.setPosition(att.getPosition());
								valAtt.setPosition(att.getPosition());
								valAtt.setBooleanValue(valAttribute.getBooleanValue());
								valAtt.setDateValue(valAttribute.getDateValue());
								valAtt.setDoubleValue(valAttribute.getDoubleValue());
								valAtt.setIntValue(valAttribute.getIntValue());
								valAtt.setStringValue(valAttribute.getStringValue());
								valAtt.setStringValues(null);

								// Normalize dates
								if (valAtt.getValue() instanceof Date)
									valAtt.setValue(ServiceUtil.convertToDate((Date) valAtt.getValue()));
								attributes.add(valAtt);
							}
						}
					}
				}
			}

			Collections.sort(attributes);
			return attributes.toArray(new GUIAttribute[0]);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}
}