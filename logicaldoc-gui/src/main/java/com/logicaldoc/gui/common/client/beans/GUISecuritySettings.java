package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Security Settings bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUISecuritySettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private int pwdSize;

	private int pwdExpiration;

	private int pwdEnforceHistory;

	private int pwdUpperCase=2;
	
	private int pwdLowerCase=2;
	
	private int pwdDigit=1;
	
	private int pwdSpecial=1;
	
	private Integer maxInactivity;

	private boolean saveLogin = false;

	private boolean ignoreLoginCase = false;

	private boolean enableAnonymousLogin = false;

	private boolean forceSsl = false;

	private boolean allowSidInRequest = false;

	private boolean alertNewDevice = true;

	private GUIUser anonymousUser = null;

	private String anonymousKey = null;

	private String geolocationKey = null;

	private String geolocationDbVer = null;

	private boolean geolocationEnabled = true;

	private boolean geolocationCache = false;

	/**
	 * Value for the Content-Security-Policy HTTP header
	 */
	private String contentSecurityPolicy = null;
	

	public int getPwdSize() {
		return pwdSize;
	}

	public void setPwdSize(int pwdSize) {
		this.pwdSize = pwdSize;
	}

	public int getPwdExpiration() {
		return pwdExpiration;
	}

	public void setPwdExpiration(int pwdExpiration) {
		this.pwdExpiration = pwdExpiration;
	}

	public boolean isSaveLogin() {
		return saveLogin;
	}

	public void setSaveLogin(boolean saveLogin) {
		this.saveLogin = saveLogin;
	}

	public GUIUser getAnonymousUser() {
		return anonymousUser;
	}

	public void setAnonymousUser(GUIUser anonymousUser) {
		this.anonymousUser = anonymousUser;
	}

	public boolean isEnableAnonymousLogin() {
		return enableAnonymousLogin;
	}

	public void setEnableAnonymousLogin(boolean enableAnonymousLogin) {
		this.enableAnonymousLogin = enableAnonymousLogin;
	}

	public boolean isIgnoreLoginCase() {
		return ignoreLoginCase;
	}

	public void setIgnoreLoginCase(boolean ignoreLoginCase) {
		this.ignoreLoginCase = ignoreLoginCase;
	}

	public boolean isForceSsl() {
		return forceSsl;
	}

	public void setForceSsl(boolean forceSsl) {
		this.forceSsl = forceSsl;
	}

	public String getAnonymousKey() {
		return anonymousKey;
	}

	public void setAnonymousKey(String anonymousKey) {
		this.anonymousKey = anonymousKey;
	}

	public boolean isAllowSidInRequest() {
		return allowSidInRequest;
	}

	public void setAllowSidInRequest(boolean allowSidInRequest) {
		this.allowSidInRequest = allowSidInRequest;
	}

	public boolean isAlertNewDevice() {
		return alertNewDevice;
	}

	public void setAlertNewDevice(boolean alertNewDevice) {
		this.alertNewDevice = alertNewDevice;
	}

	public String getGeolocationKey() {
		return geolocationKey;
	}

	public void setGeolocationKey(String geolocationKey) {
		this.geolocationKey = geolocationKey;
	}

	public String getGeolocationDbVer() {
		return geolocationDbVer;
	}

	public void setGeolocationDbVer(String geolocationDbVer) {
		this.geolocationDbVer = geolocationDbVer;
	}

	public boolean isGeolocationEnabled() {
		return geolocationEnabled;
	}

	public void setGeolocationEnabled(boolean geolocationEnabled) {
		this.geolocationEnabled = geolocationEnabled;
	}

	public boolean isGeolocationCache() {
		return geolocationCache;
	}

	public void setGeolocationCache(boolean geolocationCache) {
		this.geolocationCache = geolocationCache;
	}

	public int getPwdEnforceHistory() {
		return pwdEnforceHistory;
	}

	public void setPwdEnforceHistory(int pwdEnforceHistory) {
		this.pwdEnforceHistory = pwdEnforceHistory;
	}

	public Integer getMaxInactivity() {
		return maxInactivity;
	}

	public void setMaxInactivity(Integer maxInactivity) {
		this.maxInactivity = maxInactivity;
	}

	public String getContentSecurityPolicy() {
		return contentSecurityPolicy;
	}

	public void setContentSecurityPolicy(String contentSecurityPolicy) {
		this.contentSecurityPolicy = contentSecurityPolicy;
	}

	public int getPwdUpperCase() {
		return pwdUpperCase;
	}

	public void setPwdUpperCase(int pwdUpperCase) {
		this.pwdUpperCase = pwdUpperCase;
	}

	public int getPwdLowerCase() {
		return pwdLowerCase;
	}

	public void setPwdLowerCase(int pwdLowerCase) {
		this.pwdLowerCase = pwdLowerCase;
	}

	public int getPwdDigit() {
		return pwdDigit;
	}

	public void setPwdDigit(int pwdDigit) {
		this.pwdDigit = pwdDigit;
	}

	public int getPwdSpecial() {
		return pwdSpecial;
	}

	public void setPwdSpecial(int pwdSpecial) {
		this.pwdSpecial = pwdSpecial;
	}
}