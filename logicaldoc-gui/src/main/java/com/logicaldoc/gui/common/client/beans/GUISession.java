package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Informations about the session created on the server side.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUISession implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean loggedIn = false;
	
	/**
	 * If the session was initiated with a SSO
	 */
	private boolean singleSignOn = false;

	private String sid;

	private GUIUser user;
	
	private GUIInfo info;

	private String welcomeMessage;

	public GUIUser getUser() {
		return user;
	}

	public void setUser(GUIUser user) {
		this.user = user;
	}

	public boolean isLoggedIn() {
		return loggedIn;
	}

	public void setLoggedIn(boolean loggedIn) {
		this.loggedIn = loggedIn;
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public GUIInfo getInfo() {
		return info;
	}

	public void setInfo(GUIInfo info) {
		this.info = info;
	}

	public String getWelcomeMessage() {
		return welcomeMessage;
	}

	public void setWelcomeMessage(String welcomeMessage) {
		this.welcomeMessage = welcomeMessage;
	}
	
	public boolean isSingleSignOn() {
		return singleSignOn;
	}

	public void setSingleSignOn(boolean singleSignOn) {
		this.singleSignOn = singleSignOn;
	}
}