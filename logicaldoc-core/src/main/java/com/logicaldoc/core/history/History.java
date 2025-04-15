package com.logicaldoc.core.history;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;

/**
 * Superclass for history entries
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
@MappedSuperclass
public abstract class History extends PersistentObject implements Comparable<History> {

	public static final String ASPECT = "saveHistory";

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(History.class);

	@Column(name = "ld_date")
	private Date date = new Date();

	@Column(name = "ld_userid")
	private Long userId;

	@Column(name = "ld_username", length = 255)
	private String username = "";

	@Column(name = "ld_userlogin", length = 255)
	private String userLogin = "";

	@Column(name = "ld_event", length = 255)
	protected String event = "";

	/**
	 * Comment left in regards of this event
	 */
	@Column(name = "ld_comment")
	private String comment = "";

	@Column(name = "ld_path")
	private String path = null;

	/**
	 * Used to mark this event notified by the auditing system
	 */
	@Column(name = "ld_notified", nullable = false)
	private int notified = 0;

	@Column(name = "ld_sessionid", length = 255)
	private String sessionId = "";

	@Column(name = "ld_keylabel", length = 255)
	private String keyLabel = "";

	@Column(name = "ld_ip", length = 255)
	private String ip;

	@Column(name = "ld_geolocation", length = 255)
	private String geolocation;

	@Column(name = "ld_device", length = 255)
	private String device;

	/**
	 * Note persistent, used as convenience to store the name of the tenant
	 */
	@Transient
	private String tenant = null;

	@Transient
	private User user;

	/**
	 * Marks if this event has to be notified by the events collector
	 */
	@Transient
	private boolean notifyEvent = true;

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
	 * @return Returns the event.
	 */
	public String getEvent() {
		return event;
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
			UserDAO uDao = Context.get(UserDAO.class);
			try {
				user = uDao.findById(userId);
				uDao.initialize(user);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
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

	public Long getUserId() {
		return userId;
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
		setUser(source.getUser());
		this.event = source.event;
		setComment(source.getComment());
		setPath(source.getPath());
		setNotified(source.getNotified());
		setSessionId(source.getSessionId());
		setUserId(source.getUserId());
		setUsername(source.getUsername());
		setUserLogin(source.getUserLogin());
		setTenant(source.getTenant());
		setNotifyEvent(isNotifyEvent());
		setIp(source.getIp());
		setDevice(source.getDevice());
		setGeolocation(source.getGeolocation());
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

		for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
			String beanClassName = bd.getBeanClassName();

			try {
				Class<?> beanClass = Class.forName(beanClassName);
				Table annotation = beanClass.getAnnotation(Table.class);
				if (annotation != null && StringUtils.isNotEmpty(annotation.name())
						&& History.class.isAssignableFrom(beanClass))
					tables.add(annotation.name());
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}

		return tables;
	}
}