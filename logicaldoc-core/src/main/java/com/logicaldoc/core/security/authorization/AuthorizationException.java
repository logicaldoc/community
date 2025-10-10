package com.logicaldoc.core.security.authorization;


/**
 * A generic exception for handling unauthorized accesses
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.3
 */
public class AuthorizationException extends Exception {
	
	private static final long serialVersionUID = 1L;

	public AuthorizationException() {

	}

	public AuthorizationException(String message) {
		super(message);
	}

	public AuthorizationException(Throwable cause) {
		super(cause);
	}

	public AuthorizationException(String message, Throwable cause) {
		super(message, cause);
	}
}
