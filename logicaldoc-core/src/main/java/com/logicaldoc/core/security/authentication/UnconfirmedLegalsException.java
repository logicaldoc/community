package com.logicaldoc.core.security.authentication;

/**
 * Raised when the user did not confirm one or more legals
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class UnconfirmedLegalsException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public UnconfirmedLegalsException() {
		super("unconfirmedlegals");
	}
}