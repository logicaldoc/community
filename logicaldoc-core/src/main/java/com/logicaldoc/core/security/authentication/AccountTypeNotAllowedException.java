package com.logicaldoc.core.security.authentication;

/**
 * Raised when truing to enter with an account of a unknown type
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class AccountTypeNotAllowedException extends AuthenticationException {

	public static final String CODE = "accounttypenotallowed";

	private static final long serialVersionUID = 1L;

	public AccountTypeNotAllowedException() {
		super(CODE);
	}

	public AccountTypeNotAllowedException(Authenticator authenticator) {
		super(authenticator, CODE);
	}
}
