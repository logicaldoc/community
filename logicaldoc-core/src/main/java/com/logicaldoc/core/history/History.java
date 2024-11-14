package com.logicaldoc.core.history;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.Table;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.core.type.filter.AssignableTypeFilter;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;

/**
 * Superclass for history entries
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public abstract class History extends PersistentObject implements Comparable<History> {

	public static final String ASPECT = "saveHistory";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(History.class);

	protected Long docId;

	private Long folderId;

	private Long userId;

	private Date date = new Date();

	private String userLogin = "";

	private String username = "";

	private String event = "";

	/**
	 * Comment left in regards of this event
	 */
	private String comment = "";

	/**
	 * Something to better qualify the event
	 */
	private String reason = null;

	private String version = null;

	private String fileVersion = null;

	private String path = null;

	private String pathOld = null;

	/**
	 * Used to mark this event notified by the auditing system
	 */
	private int notified = 0;

	private String sessionId = "";

	private String keyLabel = "";

	private int isNew = 1;

	private String filename = null;

	private String filenameOld = null;

	private Long fileSize = null;

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

	// Not persistent, indicates if this event has to be notified by the events
	// collector
	private boolean notifyEvent = true;

	private String ip;

	private String geolocation;

	private String device;

	private String color;

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

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
		if (user == null && (userId != null && userId.longValue() != 0L)) {
			UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
			try {
				user = uDao.findById(userId);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
			if (user != null)
				uDao.initialize(user);
		}
		return user;
	}

	/**
	 * This setter sets the sessionId, userId, username and other informations
	 * that can be captured by the given session
	 * 
	 * @param session the session to read informations from
	 */
	public void setSession(Session session) {
		if (session != null) {
			setUser(session.getUser());
			setSessionId(session.getSid());
			setKeyLabel(session.getKeyLabel());
			setTenantId(session.getTenantId());
			setTenant(session.getTenantName());
			if (session.getClient() != null)
				setClient(session.getClient());
		}
	}

	/**
	 * This setter sets the userId and username and other informations that can
	 * be captured by the given user
	 * 
	 * @param user the user to read informations from
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

	public void setDocument(Document document) {
		this.document = document;
		if (document != null) {
			this.setTenantId(document.getTenantId());
			this.setDocId(document.getId());
			this.setFilename(document.getFileName());
			this.setFileSize(document.getFileSize());
			if (document.getFolder() != null)
				this.setFolderId(document.getFolder().getId());
			this.setVersion(document.getVersion());
			this.setFileVersion(document.getFileVersion());
			this.setColor(document.getColor());
		}
	}

	/**
	 * This setter sets the ip, device and other informations that can be
	 * captured by the given client
	 * 
	 * @param client the client to read informations from
	 */
	public void setClient(Client client) {
		if (client == null) {
			ip = null;
			geolocation = null;
			device = null;
		} else {
			ip = client.getAddress();
			device = client.getDevice() != null ? client.getDevice().toString() : null;
			geolocation = client.getGeolocation() != null ? client.getGeolocation().toString() : null;
		}
	}

	public int getIsNew() {
		return isNew;
	}

	public void setIsNew(int nnew) {
		this.isNew = nnew;
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

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
	}

	public Long getFolderId() {
		return folderId;
	}

	public Long getUserId() {
		return userId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public String getGeolocation() {
		return geolocation;
	}

	public void setGeolocation(String geolocation) {
		this.geolocation = geolocation;
	}

	public String getDevice() {
		return device;
	}

	public void setDevice(String device) {
		this.device = device;
	}

	public Long getFileSize() {
		return fileSize;
	}

	public void setFileSize(Long fileSize) {
		this.fileSize = fileSize;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public String getKeyLabel() {
		return keyLabel;
	}

	public void setKeyLabel(String keyLabel) {
		this.keyLabel = keyLabel;
	}

	protected void copyAttributesFrom(History source) {
		setTenantId(source.getTenantId());
		setDate(source.getDate());
		setCreation(source.getCreation());
		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setUser(source.getUser());
		setEvent(source.getEvent());
		setComment(source.getComment());
		setReason(source.getReason());
		setVersion(source.getVersion());
		setFileVersion(source.getFileVersion());
		setPath(source.getPath());
		setPathOld(source.getPathOld());
		setNotified(source.getNotified());
		setSessionId(source.getSessionId());
		setIsNew(source.getIsNew());
		setFilename(source.getFilename());
		setFilenameOld(source.getFilenameOld());
		setUserId(source.getUserId());
		setUsername(source.getUsername());
		setUserLogin(source.getUserLogin());
		setFile(source.getFile());
		setTenant(source.getTenant());
		setNotifyEvent(isNotifyEvent());
		setIp(source.getIp());
		setDevice(source.getDevice());
		setGeolocation(source.getGeolocation());
		setFileSize(source.getFileSize());
		setColor(source.getColor());
		setKeyLabel(source.getKeyLabel());
	}

	@Override
	public String toString() {
		return getId() + " - " + event;
	}

	@Override
	public int compareTo(History other) {
		return getDate().compareTo(other.getDate());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((date == null) ? 0 : date.hashCode());
		result = prime * result + ((docId == null) ? 0 : docId.hashCode());
		result = prime * result + ((event == null) ? 0 : event.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		History other = (History) obj;
		if (date == null) {
			if (other.date != null)
				return false;
		} else if (date.getTime() != other.date.getTime())
			return false;
		if (docId == null) {
			if (other.docId != null)
				return false;
		} else if (!docId.equals(other.docId))
			return false;
		if (event == null) {
			if (other.event != null)
				return false;
		} else if (!event.equals(other.event))
			return false;
		return true;
	}

	/**
	 * Retrieves those histories that refer to events stored in the database and
	 * used for reports. It looks for those histories that also declare the
	 * {@link Table} annotation.
	 * 
	 * @return Collection of table names
	 */
	public static Collection<String> eventTables() {
		List<String> tables = new ArrayList<>();

		ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
		scanner.addIncludeFilter(new AnnotationTypeFilter(Table.class));
		scanner.addExcludeFilter(new AssignableTypeFilter(History.class) {

			@Override
			public boolean match(MetadataReader metadataReader, MetadataReaderFactory metadataReaderFactory)
					throws IOException {
				return !super.match(metadataReader, metadataReaderFactory);
			}
		});

		for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
			String beanClassName = bd.getBeanClassName();

			try {
				Class<?> beanClass = Class.forName(beanClassName);
				Table annotation = beanClass.getAnnotation(Table.class);
				if (annotation != null && StringUtils.isNotEmpty(annotation.name()))
					tables.add(annotation.name());
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}

		return tables;
	}
}