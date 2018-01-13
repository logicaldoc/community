package com.logicaldoc.core.document;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;

/**
 * Superclass for history entries
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.0
 */
public class AbstractHistory extends PersistentObject {
	protected Long docId;

	private long folderId;

	private long userId;

	private Date date = new Date();

	private String userLogin = "";

	private String username = "";

	private String event = "";

	private String comment = "";

	private String version = null;

	private String path = null;

	private String pathOld = null;

	/**
	 * Used to mark this event notified by the auditing system
	 */
	private int notified = 0;

	private String sessionId = "";

	private int isNew = 1;

	private String filename = null;

	private String filenameOld = null;

	/**
	 * Used when storing a document
	 */
	private String file = null;

	/**
	 * Used as convenience to store the name of the tenant
	 */
	private String tenant = null;

	// Not persistent
	private User user;

	// Not persistent
	private Document document;

	// Not persistent
	private Folder folder;
	
	// Not persistent, indicates if this event has to be notified by the events collector
	private boolean notifyEvent = true;
	
	public static String ASPECT = "saveHistory";

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public AbstractHistory() {
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	/**
	 * @return Returns the date.
	 */
	public Date getDate() {
		return date;
	}

	/**
	 * @param date The date to set.
	 */
	public void setDate(Date date) {
		this.date = date;
	}

	/**
	 * @return Returns the docId.
	 */
	public Long getDocId() {
		return docId;
	}

	/**
	 * @param docId The docId to set.
	 */
	public void setDocId(Long docId) {
		this.docId = docId;
	}

	/**
	 * @return Returns the event.
	 */
	public String getEvent() {
		return event;
	}

	/**
	 * @param event The event to set.
	 */
	public void setEvent(String event) {
		this.event = event;
	}

	/**
	 * @return Returns the username.
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @param username The username to set.
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public long getFolderId() {
		return folderId;
	}

	public void setFolderId(long folderId) {
		this.folderId = folderId;
	}

	public int getNotified() {
		return notified;
	}

	public void setNotified(int notified) {
		this.notified = notified;
	}

	public String getSessionId() {
		return sessionId;
	}

	public void setSessionId(String sessionId) {
		this.sessionId = sessionId;
	}

	public User getUser() {
		if (user == null && userId != 0L) {
			UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
			user = uDao.findById(userId);
			if (user != null)
				uDao.initialize(user);
		}
		return user;
	}

	/**
	 * This setter also sets the usr
	 */
	public void setSession(Session session) {
		if (session != null) {
			setUser(session.getUser());
			setSessionId(session.getSid());
			setTenantId(session.getTenantId());
		}
	}

	/**
	 * This setter also sets the userId and username
	 */
	public void setUser(User user) {
		this.user = user;
		if (user != null) {
			setUserId(user.getId());
			setUserLogin(user.getUsername());
			setUsername(user.getFullName());
			setTenantId(user.getTenantId());
		}
	}

	public int getIsNew() {
		return isNew;
	}

	public void setIsNew(int _new) {
		this.isNew = _new;
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public String getFilenameOld() {
		return filenameOld;
	}

	public void setFilenameOld(String filenameOld) {
		this.filenameOld = filenameOld;
	}

	public String getPathOld() {
		return pathOld;
	}

	public void setPathOld(String pathOld) {
		this.pathOld = pathOld;
	}

	public String getFile() {
		return file;
	}

	public void setFile(String file) {
		this.file = file;
	}

	public String getTenant() {
		return tenant;
	}

	public void setTenant(String tenant) {
		this.tenant = tenant;
	}

	public String getUserLogin() {
		return userLogin;
	}

	public void setUserLogin(String login) {
		this.userLogin = login;
	}

	public Document getDocument() {
		return document;
	}

	public void setDocument(Document document) {
		this.document = document;
	}

	public Folder getFolder() {
		return folder;
	}

	public void setFolder(Folder folder) {
		this.folder = folder;
	}

	public boolean isNotifyEvent() {
		return notifyEvent;
	}

	public void setNotifyEvent(boolean notifyEvent) {
		this.notifyEvent = notifyEvent;
	}
}