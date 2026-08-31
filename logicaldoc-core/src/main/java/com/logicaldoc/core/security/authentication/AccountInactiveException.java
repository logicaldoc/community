package com.logicaldoc.core.security.authentication;

/**
 * Raised when the account is interpreted as inactive because it was not
 * interacting since a lot of days
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class AccountInactiveException extends AuthenticationException {
	private static final String CODE = "accountinactive";

	private static final long serialVersionUID = 1L;

	private final int days;

	public AccountInactiveException() {
		super(CODE);
		this.days = 0;
	}

	public AccountInactiveException(int days) {
		super(CODE);
		this.days = days;
	}

	public AccountInactiveException(Authenticator authenticator) {
		super(authenticator, CODE);
		this.days = 0;
	}

	@Override
	public String getMessage() {
		if (days <= 0)
			return super.getMessage();
		else
			return super.getMessage() + " - more than " + days + " days since last interaction";
	}
}