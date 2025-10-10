package com.logicaldoc.core.security.authentication;

/**
 * Raised when the specified user was not found
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AccountNotFoundException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public static final String CODE = "notfound";

	public AccountNotFoundException() {
		super(CODE);
	}

	public AccountNotFoundException(Authenticator authenticator) {
		super(authenticator, CODE);
	}

	public AccountNotFoundException(Authenticator authenticator, String message) {
		super(authenticator, message);
	}
}
