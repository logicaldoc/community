package com.logicaldoc.core.security.user;

/**
 * Possible events in the user's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public enum UserEvent {

	LOGIN("event.user.login"),
	LOGIN_FAILED("event.user.login.failed"),
	LOGOUT("event.user.logout"),
    TIMEOUT("event.user.timeout"),
    PASSWORDCHANGED("event.user.passwordchanged"),
    TWOFACHANGED("event.user.2fachanged"),
    DELETED("event.user.deleted"),
    FILE_CONVERSION("event.user.fileconversion"),
    MESSAGE_RECEIVED("event.user.messagereceived"),
    CREATED("event.user.created"),
    UPDATED("event.user.updated"),
    DISABLED("event.user.disabled"),
    ENABLED("event.user.enabled");
	
    private String event;

	UserEvent(String event) {
	    this.event = event;
	}

	@Override
	public String toString() {
	    return this.event;
	}

	public static UserEvent fromString(String event) {
	    if (event != null) {
	      for (UserEvent b : UserEvent.values()) {
	        if (event.equalsIgnoreCase(b.event)) {
	          return b;
	        }
	      }
	    }
 	    return null;
    }
}