package com.logicaldoc.core.security;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.security.dao.DeviceDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserHistoryDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * A single user session with it's unique identifier and the reference to the
 * user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.6.0
 */
public class Session extends PersistentObject implements Comparable<Session> {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(Session.class);

	public final static int STATUS_OPEN = 0;

	public final static int STATUS_EXPIRED = 1;

	public final static int STATUS_CLOSED = 2;

	private final static String ERROR = "ERROR";

	private final static String WARN = "WARN";

	private final static String INFO = "INFO";

	// Map docId - Password used to unprotect it
	private Map<Long, String> unprotectedDocs = Collections.synchronizedMap(new HashMap<Long, String>());

	private Date creation = new Date();

	private Date lastRenew = creation;

	/**
	 * Represents the auto generated identifier of the session
	 */
	private String sid;

	/**
	 * The password given by the user at login time
	 */
	private String password;

	/**
	 * A third parameter(other than the username and password) given by the
	 * client at login time
	 */
	private String key;

	private String username;

	private String tenantName;

	private String node;

	private long tenantId;

	private int status = STATUS_OPEN;

	private Client client = null;

	private User user = null;

	/**
	 * Represents a dictionary of custom informations a client may save in the
	 * session
	 */
	private transient Map<String, Object> dictionary = new ConcurrentHashMap<String, Object>();

	private transient List<Log> logs = new ArrayList<Log>();

	public Map<String, Object> getDictionary() {
		return dictionary;
	}

	public String getSid() {
		return sid;
	}

	public Date getCreation() {
		return creation;
	}

	public Date getLastRenew() {
		return lastRenew;
	}

	/**
	 * Retrieves the timeout in minutes
	 */
	protected int getTimeout() {
		int timeout = 30;
		ContextProperties config = Context.get().getProperties();		
		if (config.getInt(getTenantName() + ".session.timeout") > 0)
			timeout = config.getInt(getTenantName() + ".session.timeout");
		return timeout;
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
		 * prefer to drop milliseconds and seconds.
		 */
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(new Date());
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		Date now = calendar.getTime();

		calendar.setTime(lastRenew);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		Date lastRen = calendar.getTime();

		long diff = now.getTime() - lastRen.getTime();
		long diffMinutes = Math.abs(diff / 1000 / 60);
		return diffMinutes >= timeout;
	}

	public int getStatus() {
		return status;
	}

	protected void setExpired() {
		log.warn("Session {} expired", getSid());
		logWarn("Session expired");

		this.status = STATUS_EXPIRED;
		// Add a user history entry
		UserHistoryDAO userHistoryDAO = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		userHistoryDAO.createUserHistory(user, UserEvent.TIMEOUT.toString(), null, sid, client);
	}

	public void setClosed() {
		log.info("Session {} was closed", getSid());
		logInfo("Session closed");

		this.status = STATUS_CLOSED;
		// Add a user history entry
		UserHistoryDAO userHistoryDAO = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		userHistoryDAO.createUserHistory(user, UserEvent.LOGOUT.toString(), null, sid, client);
	}

	private Session() {

	}

	Session(User user, String password, String key, Client client) {
		super();
		assert (user != null);
		this.sid = UUID.randomUUID().toString();
		this.tenantId = user.getTenantId();
		this.user = user;
		this.username = user.getUsername();
		this.password = password;
		this.key = key;
		this.client = client;
		this.node = SystemInfo.get().getInstallationId();
		this.setLastRenew(creation);

		TenantDAO tenantDAO = (TenantDAO) Context.get().getBean(TenantDAO.class);
		this.tenantName = tenantDAO.getTenantName(tenantId);
		
		UserHistory history = saveUserHistory(user, client);

		/*
		 * Add / update the device in the DB
		 */
		UserHistoryDAO userHistoryDAO = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		if (client != null && client.getDevice() != null) {
			client.getDevice().setUserId(user.getId());
			client.getDevice().setUsername(user.getFullName());

			DeviceDAO deviceDAO = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
			Device device = deviceDAO.findByDevice(client.getDevice());
			if (device == null)
				device = client.getDevice();

			device.setUserId(user.getId());
			device.setUsername(user.getFullName());
			device.setLastLogin(creation);
			device.setIp(client.getAddress());

			try {
				boolean newDevice = device.getId() == 0L;
				deviceDAO.store(device);
				client.setDevice(device);
				history.setDevice(client.getDevice().toString());
				if (client.getGeolocation() != null)
					history.setGeolocation(client.getGeolocation().toString());
				userHistoryDAO.store(history);

				// Send an email alert to the user in case of new device
				if (newDevice && Context.get().getProperties().getBoolean(tenantName + ".alertnewdevice", true)) {
					Map<String, Object> dictionary = new HashMap<String, Object>();
					dictionary.put("user", user);
					dictionary.put("device", device);
					dictionary.put("client", client);
					dictionary.put("location", client.getGeolocation());
					dictionary.put("event", history);

					EMail email = new EMail();
					email.setTenantId(tenantId);
					email.setHtml(1);
					email.setLocale(user.getLocale());
					Recipient recipient = new Recipient();
					recipient.setAddress(user.getEmail());
					recipient.setName(user.getFullName());
					recipient.setMode(Recipient.MODE_EMAIL_TO);
					email.getRecipients().add(recipient);

					EMailSender sender = (EMailSender) Context.get().getBean(EMailSender.class);
					sender.sendAsync(email, "newdevice", dictionary);
				}

			} catch (PersistenceException e) {
				log.warn("Cannot record the device {}", device);
			}
		}

		log.info("Session {} has been started", getSid());
		logInfo("Session started");
	}

	private UserHistory saveUserHistory(User user, Client client) {
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
		UserHistoryDAO userHistoryDAO = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		UserHistory history = userHistoryDAO.createUserHistory(user, UserEvent.LOGIN.toString(), historyComment, sid,
				client);
		return history;
	}

	public String getUsername() {
		return username;
	}

	@Override
	public String toString() {
		return getSid();
	}

	@Override
	public int compareTo(Session o) {
		int compare = Integer.valueOf(status).compareTo(Integer.valueOf(o.status));
		if (compare == 0)
			compare = o.getCreation().compareTo(creation);
		return compare;
	}

	@Override
	public int hashCode() {
		return sid.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final Session other = (Session) obj;
		if (sid == null) {
			if (other.sid != null)
				return false;
		} else if (!sid.equals(other.sid))
			return false;
		return true;
	}

	public long getUserId() {
		return user.getId();
	}

	public long getTenantId() {
		return tenantId;
	}

	public String getTenantName() {
		return tenantName;
	}

	public String getPassword() {
		return password;
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

		for (Log log : logs)
			if (ERROR.equals(log.getLevel()))
				return log;
		return null;
	}

	public boolean isEmpty() {
		return logs.isEmpty();
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

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	public void setLastRenew(Date lastRenew) {
		this.lastRenew = lastRenew;
	}

	protected void setSid(String sid) {
		this.sid = sid;
	}

	protected void setStatus(int status) {
		this.status = status;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	Session getClone() {
		Session clone = new Session();
		clone.setId(getId());
		clone.setTenantId(getTenantId());
		clone.setTenantName(tenantName);
		clone.setSid(sid);
		clone.setKey(key);
		clone.setNode(node);
		clone.setUsername(username);
		clone.setPassword(password);
		clone.setCreation(creation);
		clone.setLastRenew(lastRenew);
		clone.setClient(client);
		return clone;
	}

}