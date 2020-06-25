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
	 * @param locale the currently used language specification
	 * 
	 * @return session informations
	 */
	public GUISession getSession(String locale);

	/**
	 * Changes the password of a user
	 * 
	 * @param requestorUserId The user Identifier of the requestor
	 * @param userId The user Identifier
	 * @param oldPassword can be null
	 * @param newPassword the new password
	 * @param notify If the new credentials have to be notified
	 * @return 0 if all is ok, 1 if the password is incorrect, 2 if the new
	 *         password cannot be notified, otherwise a positive number grater
	 *         than 2
	 */
	public int changePassword(Long requestorUserId, long userId, String oldPassword, String newPassword,
			boolean notify);

	/**
	 * Logs out the current user
	 */
	public void logout();

	/**
	 * Deletes a given user
	 * 
	 * @param userId identifier of the user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void deleteUser(long userId) throws ServerException;

	/**
	 * Creates or updates a user
	 * 
	 * @param user the user to save
	 * @param info informations about the User Interface
	 * 
	 * @return the saved user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIUser saveUser(GUIUser user, GUIInfo info) throws ServerException;

	/**
	 * Replicates the settings of a given user to a selection of other users
	 * 
	 * @param masterUserId identifier of the user with the settings you want to replicate
	 * @param gui if the user interface settings must be replicated
	 * @param groups if the groups must be replicated(the read-only users will not be affected by this flag)
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void replicateUsersSettings(long masterUserId, Long[] userIds, boolean gui, boolean groups) throws ServerException;
	
	/**
	 * Saves the profile data only
	 * 
	 * @param user the user to save
	 * 
	 * @return the updated user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIUser saveProfile(GUIUser user) throws ServerException;

	/**
	 * Saves the interface settings only
	 * 
	 * @param user the user to save
	 * 
	 * @return the updated user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIUser saveInterfaceSettings(GUIUser user) throws ServerException;
	
	/**
	 * Loads a given user from the database
	 * 
	 * @param userId identifier of the user
	 * 
	 * @return the user retrieved from the server application
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIUser getUser(long userId) throws ServerException;

	/**
	 * Loads a given group from the database
	 * 
	 * @param groupId identifier of the group
	 * 
	 * @return group retrieved from the server application
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIGroup getGroup(long groupId) throws ServerException;

	/**
	 * Creates or updates a group
	 * 
	 * @param group the group to save
	 * 
	 * @return the updated group
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIGroup saveGroup(GUIGroup group) throws ServerException;

	/**
	 * Deletes a given group
	 * 
	 * @param groupId identifier of the group
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void deleteGroup(long groupId) throws ServerException;

	/**
	 * Removes users from a group
	 * 
	 * @param groupId identifier of the group
	 * @param userIds array of user identifiers
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void removeFromGroup(long groupId, long[] userIds) throws ServerException;

	/**
	 * Adds a user to a group
	 * 
	 * @param groupId identifier of the group
	 * @param userId identifier of the user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void addUserToGroup(long groupId, long userId) throws ServerException;

	/**
	 * Saves security settings
	 * 
	 * @param settings settings about security
	 * 
	 * @return True if the application has to be restarted
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public boolean saveSettings(GUISecuritySettings settings) throws ServerException;

	/**
	 * Loads security settings
	 * 
	 * @return the security settings
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUISecuritySettings loadSettings() throws ServerException;

	/**
	 * Kill the session with the given sid
	 * 
	 * @param sid identifier of the session
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void kill(String sid) throws ServerException;

	/**
	 * Applies all security settings to menu
	 * 
	 * @param menu the menu
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void applyRights(GUIMenu menu) throws ServerException;

	/**
	 * Retrieves the specified menu
	 * 
	 * @param id identifier of the menu
	 * 
	 * @return the menu retrieved from the server application
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIMenu getMenu(long id) throws ServerException;

	/**
	 * Searches for users
	 * 
	 * @param username The username used in the like operator (optional)
	 * @param groupId The group ID (optional)
	 * 
	 * @return Array of found users
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIUser[] searchUsers(String username, String groupId) throws ServerException;

	/**
	 * Retrieves the list of actually blocked usernames and IPs detected as
	 * Brute Force Attack
	 * 
	 * @return the array of blocked usernames and IPs
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUISequence[] loadBlockedEntities() throws ServerException;

	/**
	 * Removes blocked entries detected as Brute Force Attack
	 * 
	 * @param id identifiers of entities from the BFA list
	 * 
	 * @throws ServerException error generated in the server application
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