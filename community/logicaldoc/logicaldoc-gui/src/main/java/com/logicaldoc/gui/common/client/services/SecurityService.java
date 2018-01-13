package com.logicaldoc.gui.common.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * The client side stub for the Security Service. This service gives all needed
 * methods to handle user sessions.
 */
@RemoteServiceRelativePath("security")
public interface SecurityService extends RemoteService {
	/**
	 * Logs-in a user by an existing session ID (session reuse)
	 *
	 */
	public GUISession getSession(String locale);

	/**
	 * Changes the password of a user
	 * 
	 * @param userId The user Identifier
	 * @param oldPassword can be null
	 * @param newPassword
	 * @param notify If the new credentials have to be notified
	 * @return 0 if all is ok, 1 if the password is incorrect, 2 if the new
	 *         password cannot be notified, otherwise a positive number grater
	 *         than 2
	 */
	public int changePassword(long userId, String oldPassword, String newPassword, boolean notify);

	/**
	 * Logs out the current user
	 */
	public void logout();

	/**
	 * Deletes a given user
	 */
	public void deleteUser(long userId) throws ServerException;

	/**
	 * Creates or updates a user
	 */
	public GUIUser saveUser(GUIUser user, GUIInfo info) throws ServerException;

	/**
	 * Saves the profile data only
	 */
	public GUIUser saveProfile(GUIUser user) throws ServerException;

	/**
	 * Loads a given user from the database
	 */
	public GUIUser getUser(long userId) throws ServerException;

	/**
	 * Loads a given group from the database
	 */
	public GUIGroup getGroup(long groupId) throws ServerException;

	/**
	 * Creates or updates a group
	 */
	public GUIGroup saveGroup(GUIGroup group) throws ServerException;

	/**
	 * Deletes a given group
	 */
	public void deleteGroup(long groupId) throws ServerException;

	/**
	 * Removes users from a group
	 */
	public void removeFromGroup(long groupId, long[] userIds) throws ServerException;

	/**
	 * Adds a user to a group
	 */
	public void addUserToGroup(long groupId, long userId) throws ServerException;

	/**
	 * Saves security settings
	 * 
	 * @return True if the application has to be restarted
	 */
	public boolean saveSettings(GUISecuritySettings settings) throws ServerException;

	/**
	 * Loads security settings
	 */
	public GUISecuritySettings loadSettings() throws ServerException;

	/**
	 * Kill the session with the given sid.
	 */
	public void kill(String sid) throws ServerException;

	/**
	 * Applies all security settings to menu
	 */
	public void applyRights(GUIMenu menu) throws ServerException;

	/**
	 * Retrieves the specified menu
	 */
	public GUIMenu getMenu(long id) throws ServerException;

	/**
	 * Searches for users
	 * 
	 * @param sid The current session ID
	 * @param username The username used in the like operator (optional)
	 * @param groupId The group ID (optional)
	 * 
	 * @return Array of found users
	 * @throws ServerException
	 */
	public GUIUser[] searchUsers(String username, String groupId) throws ServerException;

	/**
	 * Retrieves the list of actually blocked usernames and IPs detected as
	 * Brute Force Attack
	 */
	public GUISequence[] loadBlockedEntities() throws ServerException;

	/**
	 * Removes blocked entries detected as Brute Force Attack
	 */
	public void removeBlockedEntities(long[] id) throws ServerException;

	public static class Instance {
		private static SecurityServiceAsync instance;

		public static SecurityServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SecurityService.class);
			}
			return instance;
		}
	}
}