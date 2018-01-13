package com.logicaldoc.core.security;

import com.logicaldoc.core.document.AbstractHistory;

/**
 * History entry due to an event on a user.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.0
 */
public class UserHistory extends AbstractHistory {

	// Events on users
	public final static String EVENT_USER_LOGIN = "event.user.login";

	public final static String EVENT_USER_LOGIN_FAILED = "event.user.login.failed";

	public final static String EVENT_USER_LOGOUT = "event.user.logout";

	public final static String EVENT_USER_TIMEOUT = "event.user.timeout";

	public final static String EVENT_USER_PASSWORDCHANGED = "event.user.passwordchanged";
	
	public final static String EVENT_USER_2FACHANGED = "event.user.2fachanged";

	public final static String EVENT_USER_DELETED = "event.user.deleted";
	
	public final static String EVENT_FILE_CONVERSION = "event.user.fileconversion";

	private String ip;

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}
}