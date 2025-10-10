package com.logicaldoc.core.security.authentication;

/**
 * Raised when the password has expired
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class PasswordExpiredException extends AuthenticationException {

	public static final String CODE = "passwordexpired";
	
	private static final long serialVersionUID = 1L;

	public PasswordExpiredException() {
		super(CODE);
	}

	public PasswordExpiredException(Authenticator authenticator) {
		super(authenticator, CODE);
	}
}