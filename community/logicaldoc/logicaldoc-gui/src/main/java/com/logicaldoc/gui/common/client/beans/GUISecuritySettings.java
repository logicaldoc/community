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

	private boolean saveLogin = false;
	
	private boolean ignoreLoginCase = false;

	private boolean enableAnonymousLogin = false;

	private boolean forceSsl = false;

	private GUIUser anonymousUser = null;
	
	private String anonymousKey = null;
	

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
}