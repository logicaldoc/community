package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Date;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistentObject;

import eu.bitwalker.useragentutils.UserAgent;

/**
 * Represents a device with wich a user connects to the platform
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class Device extends PersistentObject implements Serializable {

	public static final String PARAM_DEVICE = "device";

	private static final long serialVersionUID = 1L;

	/**
	 * A unique identifier of the device
	 */
	private String deviceId;

	/**
	 * Name of the browser
	 */
	private String browser = "unknown";

	/**
	 * Version of the browser
	 */
	private String browserVersion;

	/**
	 * Name of the operative system
	 */
	private String operativeSystem;

	/**
	 * Type of device
	 */
	private String type;

	/**
	 * Instant of record creation
	 */
	private Date creation = new Date();

	/**
	 * Instant of last login
	 */
	private Date lastLogin;

	/**
	 * IP of last login
	 */
	private String ip;

	private long userId;

	private String username;

	private int trusted = 0;

	/**
	 * Optional label assigned to the device
	 */
	private String label;

	public Device() {

	}

	/**
	 * Constructs a new Device inspecting the current request
	 * 
	 * @param request the current request
	 */
	public Device(HttpServletRequest request) {
		UserAgent agent = UserAgent.parseUserAgentString(request.getHeader("User-Agent"));

		setDeviceId(getDeviceId(request));
		setBrowser(agent.getBrowser().getName());
		if (agent != null) {
			if (agent.getBrowserVersion() != null)
				setBrowserVersion(agent.getBrowserVersion().getVersion());
			if (agent.getOperatingSystem() != null) {
				setOperativeSystem(agent.getOperatingSystem().getName());
				if (agent.getOperatingSystem().getDeviceType() != null)
					setType(agent.getOperatingSystem().getDeviceType().toString());
			}
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

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
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
	public boolean equals(Object obj) {
		if (!(obj instanceof Device))
			return false;
		Device other = (Device) obj;
		if (deviceId != null)
			return deviceId.equals(other.getDeviceId()) || super.equals(obj);
		else
			return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return deviceId.hashCode();
	}
}