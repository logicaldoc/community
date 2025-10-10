package com.logicaldoc.core.security.authentication;

/**
 * Raised when the account exists but it is disabled
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 *
 */
public class AccountDisabledException extends AuthenticationException {

	public static final String CODE = "disabled";
	
	private static final long serialVersionUID = 1L;

	public AccountDisabledException() {
		super(CODE);
	}

	public AccountDisabledException(Authenticator authenticator) {
		super(authenticator, CODE);
	}
}