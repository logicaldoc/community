package com.logicaldoc.core.security.authentication;

import com.logicaldoc.core.PersistenceException;
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
	 * @param username the username
	 * @param password the password
	 * 
	 * @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password) throws AuthenticationException;

	/**
	 * Authenticates the user using the given credentials, if successful, the
	 * corresponding user is returned.
	 * 
	 * @param username the username
	 * @param password the password
	 * @param key Optional third authentication parameter
	 * 
	 * @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password, String key) throws AuthenticationException;

	/**
	 * Authenticates the user using the given credentials, if successful, the
	 * corresponding user is returned.
	 * 
	 * @param username the username
	 * @param password the password
	 * @param key Optional third authentication parameter
	 * 
	 * @param client Client informations
	 * 
	 * @return The user, or null if the authentication was unsuccessful
	 */
	public User authenticate(String username, String password, String key, Client client)
			throws AuthenticationException;

	/**
	 * Checks if this authenticator can authenticate a user
	 * 
	 * @param user the username to authenticate
	 * 
	 * @return true only if this authenticator can authenticate the giver
	 *         username
	 */
	public boolean canAuthenticateUser(String user);

	/**
	 * A provider can be in a disabled state
	 * 
	 * @return if this authenticator is enabled
	 */
	public boolean isEnabled();

	/**
	 * This method does not authenticate the user but just retrieves his details
	 * 
	 * @param username username of the user to find
	 * 
	 * @return object representation of the user
	 * 
	 * @throws PersistenceException Error in data layer 
	 */
	public User pickUser(String username) throws PersistenceException;
}