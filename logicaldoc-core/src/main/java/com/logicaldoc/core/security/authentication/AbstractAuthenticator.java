package com.logicaldoc.core.security.authentication;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.User;

/**
 * A place to put common methods of the authenticators
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.2
 *
 */
public abstract class AbstractAuthenticator implements Authenticator {
	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public boolean canAuthenticateUser(String username) {
		return true;
	}
	
	@Override
	public User authenticate(String username, String password, String key)
			throws com.logicaldoc.core.security.authentication.AuthenticationException {
		return authenticate(username, password, key, null);
	}

	@Override
	public User authenticate(String username, String password, String key, Client client)
			throws com.logicaldoc.core.security.authentication.AuthenticationException {
		return authenticate(username, password);
	}
}
