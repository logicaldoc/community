package com.logicaldoc.web.service;

import java.io.File;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.metadata.validation.ValidationException;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Session.Log;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authentication.InvalidSessionException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.threading.NotifyingThread;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.gui.common.client.AccessDeniedException;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.ServerValidationError;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.UploadServlet;
import com.logicaldoc.web.util.LongRunningOperationCompleteListener;
import com.logicaldoc.web.util.ServletUtil;
import com.logicaldoc.web.websockets.WebsocketTool;

/**
 * Main class for the GWT remote service implementations
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.3.3
 */
public abstract class AbstractRemoteService extends RemoteServiceServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(AbstractRemoteService.class);

	protected final String LOCALE = "locale";

	protected final String USER = "user";

	public AbstractRemoteService() {
		super();
	}

	public AbstractRemoteService(Object delegate) {
		super(delegate);
	}

	protected Map<String, File> getUploadedFiles(String sid) {
		return UploadServlet.getReceivedFiles(sid);
	}

	protected Session validateSession(HttpServletRequest request) throws InvalidSessionServerException {
		try {
			return ServletUtil.validateSession(request);
		} catch (InvalidSessionException e) {
			throw new InvalidSessionServerException(e.getMessage());
		}
	}

	/**
	 * Throws a runtime exception id the given session is invalid
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return the session
	 * 
	 * @throws InvalidSessionServerException the session does not exist or is
	 *         expired
	 */
	protected Session validateSession(String sid) throws InvalidSessionServerException {
		try {
			return ServletUtil.validateSession(sid);
		} catch (InvalidSessionException e) {
			throw new InvalidSessionServerException(e.getMessage());
		}
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
	 * @throws InvalidSessionServerException the session does not exist or is
	 *         expired
	 * @throws AccessDeniedException the user cannot access any menu
	 */
	protected Session checkEvenOneMenu(HttpServletRequest request, long... menuIds)
			throws InvalidSessionServerException, AccessDeniedException {
		try {
			return ServletUtil.checkEvenOneMenu(request, menuIds);
		} catch (InvalidSessionException e) {
			throw new InvalidSessionServerException(e.getMessage());
		} catch (ServletException e) {
			throw new AccessDeniedException(e.getMessage());
		}
	}

	/**
	 * Check if a specific menu is accessible by the user in the current session
	 * 
	 * @param request the HTTP request
	 * @param menuId identifier of the menus
	 * 
	 * @return the current session
	 * 
	 * @throws InvalidSessionServerException the session does not exist or is
	 *         expired
	 * @throws AccessDeniedException the user cannot access any menu
	 */
	protected Session checkMenu(HttpServletRequest request, long menuId)
			throws InvalidSessionServerException, AccessDeniedException {
		try {
			return ServletUtil.checkMenu(request, menuId);
		} catch (InvalidSessionException e) {
			throw new InvalidSessionServerException(e.getMessage());
		} catch (ServletException e) {
			throw new AccessDeniedException(e.getMessage());
		}
	}

	protected void checkPermission(Permission permission, User user, long folderId) throws AccessDeniedException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		try {
			if (!dao.isPermissionEnabled(permission, folderId, user.getId())) {
				String message = String.format("User %s doesn't have permission %s on folder %s", user.getUsername(),
						permission.getName(), folderId);
				throw new AccessDeniedException(message);
			}
		} catch (PersistenceException e) {
			throw new AccessDeniedException(e.getMessage());
		}
	}

	protected Locale currentLocale(Session session) throws InvalidSessionServerException {
		return (Locale) session.getDictionary().get(LOCALE);
	}

	protected Locale currentLocale(String sid) throws InvalidSessionServerException {
		Session session = validateSession(sid);
		return currentLocale(session);
	}

	protected User getSessionUser(String sid) throws InvalidSessionServerException {
		Session session = validateSession(sid);
		User user = (User) session.getDictionary().get(USER);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);
		return user;
	}

	protected User getSessionUser(HttpServletRequest request) throws InvalidSessionServerException {
		Session session = validateSession(request);
		User user = (User) session.getDictionary().get(USER);
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		userDao.initialize(user);
		return user;
	}

	protected Object throwServerException(Session session, Logger logger, Throwable t) throws ServerException {
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
			message = message.replace("com.logicaldoc.", "").replace("java.lang.", "");

		if (session != null && t != null
				&& (t instanceof org.hibernate.TransactionException || t instanceof org.hibernate.HibernateException
						|| t instanceof org.springframework.transaction.TransactionSystemException)) {
			message = I18N.message("dberrorretry", session.getUser().getLocale());
		}

		if (t instanceof ValidationException) {
			// Translate a validation error
			ValidationException ie = (ValidationException) t;
			throw new ServerValidationException(ie.getMessage(),
					ie.getErrors().values().stream()
							.map(e -> new ServerValidationError(e.getAttribute(), e.getLabel(), e.getDescription()))
							.collect(Collectors.toList()).toArray(new ServerValidationError[0]));
		} else if (t instanceof PermissionException) {
			throw new AccessDeniedException(t.getMessage());
		} else if (t instanceof ServerException) {
			throw (ServerException) t;
		} else
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
	protected Date convertToDate(Date src) {
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
	 * @throws ServerException Whatever error that may occur
	 */
	protected boolean executeLongRunningOperation(String name, Runnable runnable, Session session)
			throws ServerException {
		ThreadPools pools = (ThreadPools) Context.get().getBean(ThreadPools.class);

		/*
		 * Build the notifying thread and schedule for immediate execution (1ms
		 * delay)
		 */
		NotifyingThread task = new NotifyingThread(runnable, name);
		pools.schedule(task, "LongRunningOperations", 1);

		// Wait up to 20 seconds for completion
		while (task.getElapsedTime() < 20000 && !task.isOver()) {
			synchronized (pools) {
				try {
					pools.wait(1000);
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}

		if (task.isOver() && task.getError() != null) {
			// In case it already completed with error, re-throw the exception
			throw new ServerException(task.getError());
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
	protected GUIAttribute[] prepareGUIAttributes(Template template, ExtensibleObject extensibleObject) {
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
					att.setReadonly(templateExtAttr.getReadonly() == 1);
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
						att.setValue(convertToDate((Date) att.getValue()));

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
								GUIAttribute valAtt = new GUIAttribute(att);
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
									valAtt.setValue(convertToDate((Date) valAtt.getValue()));
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