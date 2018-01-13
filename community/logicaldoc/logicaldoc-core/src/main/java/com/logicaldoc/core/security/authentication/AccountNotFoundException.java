package com.logicaldoc.core.security.authentication;

/**
 * Raised when the specified user was not found
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AccountNotFoundException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public AccountNotFoundException() {
		super("notfound");
	}

	public AccountNotFoundException(String message) {
		super(message);
	}

}
