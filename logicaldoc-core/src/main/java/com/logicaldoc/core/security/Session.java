package com.logicaldoc.core.security;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

/**
 * A single user session with it's unique identifier and the reference to the
 * user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6.0
 */
@Entity
@Table(name = "ld_session")
@Cacheable
public class Session extends PersistentObject implements Comparable<Session> {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(Session.class);

	public static final int STATUS_OPEN = 0;

	public static final int STATUS_EXPIRED = 1;

	public static final int STATUS_CLOSED = 2;

	private static final String ERROR = "ERROR";

	private static final String WARN = "WARN";

	private static final String INFO = "INFO";

	// Map docId - Password used to not protect it
	@Transient
	private Map<Long, String> unprotectedDocs = Collections.synchronizedMap(new HashMap<>());

	@Column(name = "ld_lastrenew", columnDefinition = "DATETIME(3)")
	private Date lastRenew = new Date();

	@Column(name = "ld_finished", columnDefinition = "DATETIME(3)")
	private Date finished;

	/**
	 * Represents the auto generated identifier of the session
	 */
	@Column(name = "ld_sid", length = 255, nullable = false)
	private String sid;

	/**
	 * A third parameter(other than the username and password) given by the
	 * client at login time, or an API Key generated in LogicalDOC.
	 */
	@Column(name = "ld_key", length = 255)
	private String key;

	@Transient
	private String decodedKey;

	/**
	 * A human readable visualization of part of the key
	 */
	@Column(name = "ld_keylabel", length = 255)
	private String keyLabel;

	@Column(name = "ld_username", length = 255)
	private String username;

	@Column(name = "ld_tenantname", length = 255)
	private String tenantName;

	@Column(name = "ld_node", length = 255)
	private String node;

	@Column(name = "ld_tenantid", nullable = false)
	private long tenantId;

	@Column(name = "ld_status", nullable = false)
	private int status = STATUS_OPEN;

	@Embedded
	private Client client = null;

	@Transient
	private User user = null;

	/**
	 * Represents a dictionary of custom informations a client may save in the
	 * session
	 */
	@Transient
	private transient Map<String, Object> dictionary = new ConcurrentHashMap<>();

	@Transient
	private transient List<Log> logs = new ArrayList<>();

	public Map<String, Object> getDictionary() {
		return dictionary;
	}

	public String getSid() {
		return sid;
	}

	public Date getLastRenew() {
		return lastRenew;
	}

	/**
	 * Retrieves the timeout in minutes
	 */
	protected int getTimeout() {
		return Context.get().getProperties().getInt(getTenantName() + ".session.timeout", -1);
	}

	public boolean isOpen() {
		return status == STATUS_OPEN;
	}

	protected boolean isTimedOut() {
		if (status != STATUS_OPEN)
			return true;

		int timeout = getTimeout();
		if (timeout <= 0)
			return false;

		/**
		 * In order to compare the actual date and the last renewal date, we
		 * prefer to drop milliseconds.
		 */
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(new Date());
		calendar.set(Calendar.MILLISECOND, 0);
		Date now = calendar.getTime();

		calendar.setTime(lastRenew);
		calendar.set(Calendar.MILLISECOND, 0);
		Date lastRen = calendar.getTime();

		long diff = Math.abs(now.getTime() - lastRen.getTime());
		long diffMinutes = TimeUnit.MILLISECONDS.toMinutes(diff);
		return diffMinutes >= timeout;
	}

	public int getStatus() {
		return status;
	}

	protected void setExpired() {
		log.warn("Session {} expired", getSid());
		logWarn("Session expired");

		this.status = STATUS_EXPIRED;
		this.finished = new Date();

		// Add a user history entry
		UserHistoryDAO.get().createUserHistory(user, UserEvent.TIMEOUT, null, sid, client);
	}

	public void setClosed() {
		log.info("Session {} was closed", getSid());
		logInfo("Session closed");

		this.status = STATUS_CLOSED;
		this.finished = new Date();

		// Add a user history entry
		UserHistoryDAO.get().createUserHistory(user, UserEvent.LOGOUT, null, sid, client);
	}

	public String getDecodedKey() {
		return decodedKey;
	}

	/**
	 * Sets the key and encode it
	 * 
	 * @param decodedKey The key in readable format
	 * @throws NoSuchAlgorithmException Cripting error
	 */
	public void setDecodedKey(String decodedKey) throws NoSuchAlgorithmException {
		if (StringUtils.isNotEmpty(decodedKey)) {
			this.decodedKey = decodedKey;
			this.key = CryptUtil.encryptSHA256(decodedKey);

			String abbreviation = decodedKey.length() > 14 ? StringUtils.right(decodedKey, 4) : "";
			this.keyLabel = decodedKey.length() < 10 ? "..." : StringUtils.abbreviate(decodedKey, 10) + abbreviation;
		}
	}

	@SuppressWarnings("unused")
	private Session() {
		// Just o avoid standard constructor
	}

	Session(User user, String key, Client client) {
		super();

		if (user == null)
			throw new IllegalArgumentException("user cannot be null");

		this.sid = UUID.randomUUID().toString();
		this.tenantId = user.getTenantId();
		this.user = user;
		this.username = user.getUsername();
		try {
			setDecodedKey(key);
		} catch (NoSuchAlgorithmException e) {
			log.warn("Cannot save the key", e);
		}
		this.client = client;
		this.node = SystemInfo.get().getInstallationId();
		this.setLastRenew(getCreation());

		try {
			this.tenantName = TenantDAO.get().getTenantName(tenantId);
		} catch (PersistenceException e) {
			log.warn("Cannot retrieve the name of tenant {}", tenantId);
		}

		UserHistory history = saveLoginEvent(user, client);

		/*
		 * Add / update the device in the DB
		 */
		if (client != null && client.getDevice() != null) {
			client.getDevice().setUserId(user.getId());
			client.getDevice().setUsername(user.getFullName());

			Device device = DeviceDAO.get().findByDevice(client.getDevice());
			if (device == null)
				device = client.getDevice();

			device.setUserId(user.getId());
			device.setUsername(user.getFullName());
			device.setLastLogin(getCreation());
			device.setIp(client.getAddress());

			try {
				boolean newDevice = device.getId() == 0L;
				DeviceDAO.get().store(device);
				client.setDevice(device);
				history.setDevice(client.getDevice().toString());
				if (client.getGeolocation() != null)
					history.setGeolocation(client.getGeolocation().toString());
				UserHistoryDAO.get().store(history);

				// Send an email alert to the user in case of new device
				if (newDevice && Context.get().getProperties().getBoolean(tenantName + ".alertnewdevice", true)) {
					Map<String, Object> dictionaryMap = new HashMap<>();
					dictionaryMap.put("user", user);
					dictionaryMap.put("device", device);
					dictionaryMap.put("client", client);
					dictionaryMap.put("location", client.getGeolocation());
					dictionaryMap.put("event", history);

					EMail email = new EMail();
					email.setTenantId(tenantId);
					email.setHtml(true);
					email.setLocale(user.getLocale());
					Recipient recipient = new Recipient();
					recipient.setAddress(user.getEmail());
					recipient.setName(user.getFullName());
					recipient.setMode(Recipient.MODE_EMAIL_TO);
					email.getRecipients().add(recipient);

					EMailSender.get().sendAsync(email, "newdevice", dictionaryMap);
				}
			} catch (PersistenceException e) {
				log.warn("Cannot record the device {}", device);
			}
		}

		log.info("Session {} has been started", getSid());
		logInfo("Session started");
	}

	Session(Session other) {
		this.setId(other.getId());
		this.setTenantId(other.getTenantId());
		this.setTenantName(other.tenantName);
		this.setSid(other.sid);
		this.key = other.key;
		this.keyLabel = other.keyLabel;
		this.decodedKey = other.decodedKey;
		this.setNode(other.node);
		this.username = other.username;
		this.setCreation(other.getCreation());
		this.setLastRenew(other.lastRenew);
		this.setClient(other.client);
	}

	private UserHistory saveLoginEvent(User user, Client client) {
		/*
		 * The history comment the remote host and IP
		 */
		String historyComment = "";
		if (client != null) {
			String addr = client.getAddress();
			String host = client.getHost();
			if (StringUtils.isNotEmpty(host) && !host.equals(addr))
				historyComment = host + " (" + addr + ") ";
			else
				historyComment = addr;
		}

		// Add a user history entry
		UserHistoryDAO userHistoryDAO = UserHistoryDAO.get();
		UserHistory history = UserHistoryDAO.get().createUserHistory(user, UserEvent.LOGIN, historyComment, sid, client);

		// Update the last login into the DB
		try {
			user.setLastLogin(history.getDate());
			userHistoryDAO.jdbcUpdate("update ld_user set ld_lastlogin = :lastLogin where ld_id = :userId",
					Map.of("lastLogin", user.getLastLogin(), "userId", user.getId()));
		} catch (PersistenceException e) {
			log.warn("Last login of user {} not saved", user.getUsername());
		}

		return history;
	}

	public String getUsername() {
		return username;
	}

	@Override
	public String toString() {
		return getSid();
	}

	public long getUserId() {
		return user.getId();
	}

	@Override
	public long getTenantId() {
		return tenantId;
	}

	public String getTenantName() {
		return tenantName;
	}

	public void logError(String message) {
		logs.add(0, new Log(ERROR, message));
	}

	public void logWarn(String message) {
		logs.add(0, new Log(WARN, message));
	}

	public void logInfo(String message) {
		logs.add(0, new Log(INFO, message));
	}

	public List<Log> getLogs() {
		return logs;
	}

	public Log getLastError() {
		if (logs == null || logs.isEmpty())
			return null;

		for (Log lg : logs)
			if (ERROR.equals(lg.getLevel()))
				return lg;
		return null;
	}

	public boolean isEmpty() {
		return logs.isEmpty();
	}

	/**
	 * Retrieves the total duration of the session
	 * 
	 * @return the duration in milliseconds
	 */
	public long getDuration() {
		return Math.abs(
				getCreation().getTime() - (getFinished() != null ? getFinished().getTime() : new Date().getTime()));
	}

	public class Log {
		private Date date = new Date();

		private String level;

		private String message;

		public Log(String level, String message) {
			super();
			this.level = level;
			this.message = message;
		}

		@Override
		public String toString() {
			return message;
		}

		public String getLevel() {
			return level;
		}

		public Date getDate() {
			return date;
		}

		public String getMessage() {
			return message;
		}
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public Client getClient() {
		return client;
	}

	void setClient(Client client) {
		this.client = client;
	}

	public User getUser() {
		return user;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	@Override
	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public void setTenantName(String tenantName) {
		this.tenantName = tenantName;
	}

	public Map<Long, String> getUnprotectedDocs() {
		return unprotectedDocs;
	}

	public String getNode() {
		return node;
	}

	public void setNode(String node) {
		this.node = node;
	}

	public void setLastRenew(Date lastRenew) {
		this.lastRenew = lastRenew;
	}

	public Date getFinished() {
		return finished;
	}

	public void setFinished(Date finished) {
		this.finished = finished;
	}

	protected void setSid(String sid) {
		this.sid = sid;
	}

	protected void setStatus(int status) {
		this.status = status;
	}

	public String getKeyLabel() {
		return keyLabel;
	}

	public void setKeyLabel(String keyLabel) {
		this.keyLabel = keyLabel;
	}

	@Override
	public int compareTo(Session other) {
		if (equals(other))
			return 0;
		int compare = Integer.compare(status, other.status);
		if (compare == 0)
			compare = other.getCreation().compareTo(getCreation());
		return compare;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((sid == null) ? 0 : sid.hashCode());
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
		Session other = (Session) obj;
		if (sid == null) {
			if (other.sid != null)
				return false;
		} else if (!sid.equals(other.sid))
			return false;
		return true;
	}
}