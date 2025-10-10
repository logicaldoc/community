package com.logicaldoc.core.security.authentication;

/**
 * Raised when given password is not correct
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class WrongPasswordException extends AuthenticationException {

	public static final String CODE = "wrongpassword";

	private static final long serialVersionUID = 1L;

	public WrongPasswordException() {
		super(CODE);
	}

	public WrongPasswordException(Authenticator authenticator) {
		super(authenticator, CODE);
	}
}