package com.logicaldoc.core.security.authentication;

/**
 * Raised when the account exists but it is disabled
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 *
 */
public class AccountDisabledException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public AccountDisabledException() {
		super("disabled");
	}

	public AccountDisabledException(String message) {
		super(message);
	}

}
