package com.logicaldoc.core.security.authentication;

/**
 * A generic exception during the authentication process
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AuthenticationException extends RuntimeException{

	private static final long serialVersionUID = 1L;

	public AuthenticationException() {
		super();
	}

	public AuthenticationException(String message) {
		super(message);
	}
}
