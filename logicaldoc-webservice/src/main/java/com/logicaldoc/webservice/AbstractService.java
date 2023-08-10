package com.logicaldoc.webservice;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.xml.ws.WebServiceContext;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.apache.cxf.transport.http.AbstractHTTPDestination;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.authentication.AccountNotFoundException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authentication.InvalidSessionException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.model.WSUtil;

/**
 * Basepoint for creating webservices implementations
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class AbstractService {

	private static final String FOLDER = "folder ";

	private static Logger log = LoggerFactory.getLogger(AbstractService.class);

	private boolean validateSession = true;

	public void setValidateSession(boolean validateSession) {
		this.validateSession = validateSession;
	}

	@Resource
	protected WebServiceContext context;

	@Resource
	protected MessageContext messageContext;

	/**
	 * Utility method that validates the session and retrieve the associated
	 * user
	 * 
	 * @param sid The session identifier
	 * @return the user in session
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws WebserviceException in case the webservices are not enabled
	 * @throws AuthenticationException the given session is invalid
	 */
	protected User validateSession(String sid)
			throws WebserviceException, PersistenceException, AuthenticationException {
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		if (!validateSession) {
			User user = new User();
			user.setId(1L);
			user.setTenantId(1L);
			user.setName("admin");
			Set<Group> groups = new HashSet<>();
			GroupDAO grpDao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			groups.add(grpDao.findById(1));
			user.setGroups(groups);
			return user;
		}

		if (!isWebserviceEnabled())
			throw new WebserviceException("WebServices are disabled");

		if (sid == null || !SessionManager.get().isOpen(sid)) {
			throw new InvalidSessionException(sid);
		} else {
			SessionManager.get().renew(sid);
		}
		String username = SessionManager.get().get(sid).getUsername();
		User user = userDao.findByUsername(username);
		if (user == null)
			throw new AccountNotFoundException(null, String.format("User %s not found", username));
		else
			userDao.initialize(user);
		return user;
	}

	/**
	 * Checks if the current user belongs to a group
	 * 
	 * @throws PersistenceException error at data layer
	 * 
	 * @throws WebserviceException the user is not member of group
	 * 
	 */
	protected void checkGroup(String sid, String group) throws WebserviceException, PersistenceException {
		User user = validateSession(sid);
		if (!user.isMemberOf(group)) {
			String message = String.format("User %s doesn't belong to group %s", user.getUsername(), group);
			log.error(message);
			throw new WebserviceException(message);
		}
	}

	/**
	 * Checks if the current user is an administrator (group admin).
	 * 
	 * @throws PersistenceException error at data layer
	 * 
	 * @throws WebserviceException the user is not member of admin
	 */
	protected void checkAdministrator(String sid) throws WebserviceException, PersistenceException {
		checkGroup(sid, "admin");
	}

	protected void checkMenu(String sid, long menuId)
			throws WebserviceException, PersistenceException, PermissionException {
		User user = validateSession(sid);
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!dao.isReadEnable(menuId, user.getId())) {
			String message = String.format("User %s cannot access menu %s", user.getUsername(), menuId);
			log.error(message);
			throw new PermissionException(user.getUsername(), "menu " + menuId, "access");
		}
	}

	protected void checkPermission(Permission permission, User user, long folderId)
			throws PersistenceException, PermissionException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(permission, folderId, user.getId())) {
			String message = String.format("User %s doesn't have permission %s on folder %s", user.getUsername(),
					permission.getName(), folderId);
			log.error(message);
			throw new PermissionException(user.getUsername(), FOLDER + folderId, permission);
		}
	}

	protected void checkMenu(User user, long menuId) throws PermissionException {
		MenuDAO dao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		if (!dao.isReadEnable(menuId, user.getId())) {
			String message = String.format("User %s doesn't have read permission on menu %s", user.getUsername(),
					menuId);
			log.error(message);
			throw new PermissionException(user.getUsername(), "menu " + menuId, Permission.READ);
		}
	}

	protected void checkWriteEnable(User user, long folderId) throws PermissionException, PersistenceException {
		checkPermission(Permission.WRITE, user, folderId);
	}

	protected void checkReadEnable(User user, long folderId) throws PersistenceException, PermissionException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isReadEnabled(folderId, user.getId())) {
			String message = String.format("User %s doesn't have read permission on folder %s", user.getUsername(),
					folderId);
			log.error(message);
			throw new PermissionException(user.getUsername(), FOLDER + folderId, Permission.READ);
		}
	}

	protected void checkDownloadEnable(User user, long folderId) throws PersistenceException, PermissionException {
		FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		if (!dao.isPermissionEnabled(Permission.DOWNLOAD, folderId, user.getId())) {
			String message = String.format("User %s doesn't have download permission on folder %s", user.getUsername(),
					folderId);
			log.error(message);
			throw new PermissionException(user.getUsername(), FOLDER + folderId, Permission.DOWNLOAD);
		}
	}

	protected void checkPublished(User user, Document doc) throws WebserviceException {
		if (!user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher") && !doc.isPublishing())
			throw new WebserviceException("Document not published");
	}

	protected void checkNotArchived(Document doc) throws WebserviceException {
		if (doc.getStatus() == AbstractDocument.DOC_ARCHIVED)
			throw new WebserviceException("Document is archived");
	}

	protected boolean isWebserviceEnabled() {
		return "true".equals(Context.get().getProperties().get("webservice.enabled"));
	}

	public WebServiceContext getContext() {
		return context;
	}

	public void setContext(WebServiceContext context) {
		this.context = context;
	}

	public MessageContext getMessageContext() {
		return messageContext;
	}

	@javax.ws.rs.core.Context
	public void setMessageContext(MessageContext messageContext) {
		// https://docs.oracle.com/cd/E13222_01/wls/docs92/webserv/annotations.html
		// https://jersey.java.net/documentation/latest/jaxrs-resources.html#d0e2790
		// https://jersey.java.net/apidocs-javax.jax-rs/2.0.1/javax/ws/rs/core/Context.html
		this.messageContext = messageContext;
	}

	/**
	 * Gets the current Session ID following this logic:
	 * <ol>
	 * <li>Request parameter sid</li>
	 * <li>Request attribute sid</li>
	 * <li>Session attribute sid</li>
	 * <li>Request cookie ldoc-sid</li>
	 * <li>SecurityContextHolder</li>
	 * </ol>
	 * 
	 * @return The current Session ID
	 */
	protected String getCurrentSessionId() {
		HttpServletRequest request = null;
		if (context != null && context.getMessageContext() != null)
			request = (HttpServletRequest) context.getMessageContext().get(AbstractHTTPDestination.HTTP_REQUEST);
		else if (messageContext != null)
			request = (HttpServletRequest) messageContext.get(AbstractHTTPDestination.HTTP_REQUEST);

		Session session = SessionManager.get().getSession(request);

		if (session != null)
			return session.getSid();
		return null;
	}

	/**
	 * Same as getCurrentSessionId but throws an Exception in case of bad
	 * session
	 * 
	 * @return The session ID (if valid)
	 * 
	 * @throws InvalidSessionException the session is not valid
	 */
	protected String validateSession() throws InvalidSessionException {
		if (validateSession) {
			String sid = getCurrentSessionId();
			if (sid == null || !SessionManager.get().isOpen(sid)) {
				throw new InvalidSessionException(sid);
			} else {
				SessionManager.get().renew(sid);
			}
			return sid;
		} else
			return null;
	}

	public static String convertDateToString(Date date) {
		return DateUtil.format(date);
	}

	public static Date convertStringToDate(String date) {
		return WSUtil.convertStringToDate(date);
	}

	public boolean isValidateSession() {
		return validateSession;
	}
}