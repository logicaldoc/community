package com.logicaldoc.core.security.authentication;

/**
 * Raised when someone tries to impersonate another user without permission
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public class ForbiddenImpersonationException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public ForbiddenImpersonationException() {
		super();
	}

	public ForbiddenImpersonationException(String username, String usernameToImpersonate) {
		super(String.format("User %s not allowed to impersonate %s", username, usernameToImpersonate));
	}
}