package com.logicaldoc.core.security.authentication;

/**
 * A generic exception during the authentication process
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AuthenticationException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	protected Authenticator authenticator;

	public AuthenticationException() {
		super();
	}

	public AuthenticationException(Authenticator authenticator) {
		this.authenticator = authenticator;
	}

	public AuthenticationException(Authenticator authenticator, String code) {
		super(code);
		this.authenticator = authenticator;
	}

	public AuthenticationException(String message) {
		this(null, message);
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