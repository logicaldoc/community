package com.logicaldoc.core.security.authentication;

/**
 * Raised when the user is trying to enter outside the working time
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class OutsideWorkingTimeException extends AuthenticationException {

	public static final String CODE = "outsideworkingtime";
	
	private static final long serialVersionUID = 1L;

	public OutsideWorkingTimeException() {
		super(CODE);
	}

	public OutsideWorkingTimeException(Authenticator authenticator) {
		super(authenticator, CODE);
	}
}