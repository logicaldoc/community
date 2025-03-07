package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.PersistentObject;

import eu.bitwalker.useragentutils.UserAgent;

/**
 * Represents a device with wich a user connects to the platform
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
@Entity
@Table(name = "ld_device")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Device extends PersistentObject implements Serializable {

	public static final String PARAM_DEVICE = "device";

	private static final long serialVersionUID = 1L;

	/**
	 * A unique identifier of the device
	 */
	@Column(name = "ld_deviceid", length = 255, nullable = false)
	private String deviceId;

	@Column(name = "ld_userid")
	private long userId;
	
	@Column(name = "ld_username", length = 255)
	private String username;
	
	/**
	 * Name of the browser
	 */
	@Column(name = "ld_browser", length = 255)
	private String browser = "unknown";
	
	/**
	 * Version of the browser
	 */
	@Column(name = "ld_browserversion", length = 255)
	private String browserVersion;

	/**
	 * Name of the operative system
	 */
	@Column(name = "ld_operativesystem", length = 255)
	private String operativeSystem;

	/**
	 * Type of device
	 */
	@Column(name = "ld_type", length = 255)
	private String type;

	/**
	 * Instant of last login
	 */
	@Column(name = "ld_lastlogin")
	private Date lastLogin;

	@Column(name = "ld_trusted", nullable = false)
	private int trusted = 0;
	
	/**
	 * IP of last login
	 */
	@Column(name = "ld_ip", length = 255)
	private String ip;

	/**
	 * Optional label assigned to the device
	 */
	@Column(name = "ld_label", length = 255)
	private String label;

	public Device() {

	}

	/**
	 * Constructs a new Device inspecting the current request
	 * 
	 * @param request the current request
	 */
	public Device(HttpServletRequest request) {
		setDeviceId(getDeviceId(request));

		UserAgent agent = UserAgent.parseUserAgentString(request.getHeader("User-Agent"));
		setBrowser(agent.getBrowser().getName());

		if (agent.getBrowserVersion() != null)
			setBrowserVersion(agent.getBrowserVersion().getVersion());
		if (agent.getOperatingSystem() != null) {
			setOperativeSystem(agent.getOperatingSystem().getName());
			if (agent.getOperatingSystem().getDeviceType() != null)
				setType(agent.getOperatingSystem().getDeviceType().toString());
		}

		setIp(request.getRemoteAddr());
	}

	public String getBrowser() {
		return browser;
	}

	public void setBrowser(String browser) {
		this.browser = browser;
	}

	public String getBrowserVersion() {
		return browserVersion;
	}

	public void setBrowserVersion(String browserVersion) {
		this.browserVersion = browserVersion;
	}

	public String getOperativeSystem() {
		return operativeSystem;
	}

	public void setOperativeSystem(String operativeSystem) {
		this.operativeSystem = operativeSystem;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Date getLastLogin() {
		return lastLogin;
	}

	public void setLastLogin(Date lastLogin) {
		this.lastLogin = lastLogin;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public int getTrusted() {
		return trusted;
	}

	public void setTrusted(int trusted) {
		this.trusted = trusted;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getBrowser());
		if (StringUtils.isNotEmpty(getBrowserVersion())) {
			sb.append(" v");
			sb.append(getBrowserVersion());
		}
		if (StringUtils.isNotEmpty(getOperativeSystem())) {
			sb.append(" on ");
			sb.append(getOperativeSystem());
		}
		if (StringUtils.isNotEmpty(getType())) {
			sb.append(" (");
			sb.append(getType());
			sb.append(")");
		}
		return sb.toString();
	}

	/**
	 * Retrieves the device's ID specification from the parameter
	 * {@link Device#PARAM_DEVICE} or the cookie
	 * ldoc-{@link Device#COOKIE_DEVICE}
	 * 
	 * @param request the current request
	 * 
	 * @return the found device specification
	 */
	private static String getDeviceId(HttpServletRequest request) {
		if (request.getParameter(PARAM_DEVICE) != null)
			return request.getParameter(PARAM_DEVICE);

		if (request.getAttribute(PARAM_DEVICE) != null)
			return request.getAttribute(PARAM_DEVICE).toString();

		Cookie[] cookies = request.getCookies();
		if (cookies != null)
			for (Cookie cookie : cookies) {
				if (("ldoc-" + PARAM_DEVICE).contentEquals(cookie.getName()))
					return cookie.getValue();
			}

		return null;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((deviceId == null) ? 0 : deviceId.hashCode());
		result = prime * result + (int) (userId ^ (userId >>> 32));
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
		Device other = (Device) obj;
		if (deviceId == null) {
			if (other.deviceId != null)
				return false;
		} else if (!deviceId.equals(other.deviceId))
			return false;
		return userId == other.userId;
	}
}