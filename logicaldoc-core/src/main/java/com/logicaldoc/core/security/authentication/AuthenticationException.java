package com.logicaldoc.core.security.authentication;

/**
 * A generic exception during the authentication process
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AuthenticationException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	private final transient Authenticator authenticator;

	public AuthenticationException() {
		super();
		authenticator = null;
	}

	public AuthenticationException(Authenticator authenticator) {
		this.authenticator = authenticator;
	}

	public AuthenticationException(Authenticator authenticator, String code) {
		super(code);
		this.authenticator = authenticator;
	}
	
	public AuthenticationException(Authenticator authenticator, String code, Throwable cause) {
		super(code, cause);
		this.authenticator = authenticator;
	}

	public AuthenticationException(String message) {
		this(null, message);
	}
	
	public AuthenticationException(String message, Throwable cause) {
		this(null, message, cause);
	}

	public Authenticator getAuthenticator() {
		return authenticator;
	}

	public boolean mustRecordFailure() {
		return true;
	}

	@Override
	public String toString() {
		String baseString = super.toString().replace("com.logicaldoc.", "");
		if (authenticator == null)
			return baseString;
		else
			return authenticator.getClass().getSimpleName() + " > " + baseString;

	}
}