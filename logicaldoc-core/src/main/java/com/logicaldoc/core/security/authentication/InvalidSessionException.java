package com.logicaldoc.core.security.authentication;

/**
 * Raised when a session is not valid
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class InvalidSessionException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	private String sid;

	public InvalidSessionException() {
	}

	public InvalidSessionException(String sid) {
		super(String.format("Invalid session %s", sid));
		this.sid = sid;
	}

	public String getSid() {
		return sid;
	}
}