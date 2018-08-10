package com.logicaldoc.core.security.authentication;

import com.logicaldoc.core.security.authentication.AuthenticationException;

/**
 * Raised when the remote client is in the black list
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 *
 */
public class IPBlockedException extends AuthenticationException {

	private static final long serialVersionUID = 1L;

	public IPBlockedException() {
		super("ipblocked");
	}

	public IPBlockedException(String message) {
		super(message);
	}
}