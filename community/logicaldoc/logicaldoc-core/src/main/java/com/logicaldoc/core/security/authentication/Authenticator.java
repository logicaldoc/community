package com.logicaldoc.core.security.authentication;

import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.User;

/**
 * Implementations of this interface must provide authentication of a user
 * against a defined source.
 * 
 * @author Sebastian Wenzky
 * @since 4.5
 */
public interface Authenticator {

	/**
	 * Authenticates the user using the given credentials, if successful, the
	 * corresponding user is returned.
	 * 
	 * @param username
	 * @param password
	 * @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password) throws AuthenticationException;

	/**
	 * Authenticates the user using the given credentials, if successful, the
	 * corresponding user is returned.
	 * 
	 * @param username
	 * @param password
	 * @param key Optional third authentication parameter
	 * @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password, String key) throws AuthenticationException;

	/**
	 * Authenticates the user using the given credentials, if successful, the
	 * corresponding user is returned.
	 * 
	 * @param username
	 * @param password
	 * @param key Optional third authentication parameter
	 * @param client Client informations
	 * @return @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password, String key, Client client) throws AuthenticationException;

	public boolean canAuthenticateUser(String user);

	/**
	 * A provider can be in a disabled state
	 */
	public boolean isEnabled();
}
