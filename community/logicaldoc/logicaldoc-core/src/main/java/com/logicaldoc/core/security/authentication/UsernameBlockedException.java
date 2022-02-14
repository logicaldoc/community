package com.logicaldoc.core.security.authentication;

/**
 * Raised when the remote client is in the black list
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 *
 */
public class UsernameBlockedException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public UsernameBlockedException() {
		super("usernameblocked");
	}
}