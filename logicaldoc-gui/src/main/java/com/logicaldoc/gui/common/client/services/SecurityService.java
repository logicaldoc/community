package com.logicaldoc.gui.common.client.services;

import java.util.List;

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
import com.logicaldoc.gui.common.client.beans.GUIValue;

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
	 * @param sid the session ID (optional), if not provided it is taken by
	 *        cookies
	 * 
	 * @return session informations
	 */
	public GUISession getSession(String locale, String sid);

	/**
	 * Changes the password of a user
	 * 
	 * @param requestorUserId The user Identifier of the requestor
	 * @param userId The user Identifier
	 * @param oldPassword can be null
	 * @param newPassword the new password
	 * @param notify If the new credentials have to be notified
	 * 
	 * @return the error code and message. 0 if all went ok, 1 if the password
	 *         is incorrect, 2 if the new password cannot be notified, 3 if the
	 *         password has been already used, otherwise a positive number
	 *         grater than 3
	 */
	public GUIValue changePassword(Long requestorUserId, long userId, String oldPassword, String newPassword,
			boolean notify);

	/**
	 * Generates a password using the configured policies.
	 * 
	 * @return the generated password
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public String generatePassword() throws ServerException;

	/**
	 * Generates a password
	 * 
	 * @param length dimension of the password
	 * @param uppercaseChars minimum number of upper case chars
	 * @param lowercaseChars minimum number of lower case chars
	 * @param digits minimum number of digits
	 * @param specialChars minimum number of special chars
	 * @param maxSequenceSize maximum size of a sequence
	 * @param maxOccurrences maximum number of occurrences of the same char
	 * 
	 * @return the generated password
	 */
	public String generatePassword2(int length, int uppercaseChars, int lowercaseChars, int digits, int specialChars,
			int maxSequenceSize, int maxOccurrences);

	/**
	 * Validates a password
	 * 
	 * @param password the password to validate
	 * @param length dimension of the password
	 * @param uppercaseChars minimum number of upper case chars
	 * @param lowercaseChars minimum number of lower case chars
	 * @param digits minimum number of digits
	 * @param specialChars minimum number of special chars
	 * @param maxSequenceSize maximum size of a sequence
	 * @param maxOccurrences maximum number of occurrences of the same char
	 * 
	 * @return the reasons for the failure or empty
	 */
	public List<String> validatePassword(String password, int length, int uppercaseChars, int lowercaseChars,
			int digits, int specialChars, int maxSequenceSize, int maxOccurrences);

	/**
	 * Changes the status of a user
	 * 
	 * @param userId The user Identifier
	 * @param enabled If the user must be enabled or not
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void changeStatus(long userId, boolean enabled) throws ServerException;

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
	 * @param masterUserId identifier of the user with the settings you want to
	 *        replicate
	 * @param userIds identifiers of the users to replicate the settings to
	 * @param gui if the user interface settings must be replicated
	 * @param groups if the groups must be replicated(the read-only users will
	 *        not be affected by this flag)
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void replicateUsersSettings(long masterUserId, List<Long> userIds, boolean gui, boolean groups)
			throws ServerException;

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
	 * @param userIds user identifiers
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void removeFromGroup(long groupId, List<Long> userIds) throws ServerException;

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
	public void saveACL(GUIMenu menu) throws ServerException;

	/**
	 * Retrieves the specified menu
	 * 
	 * @param id identifier of the menu
	 * @param locale currently selected locale
	 * 
	 * @return the menu retrieved from the server application
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIMenu getMenu(long id, String locale) throws ServerException;

	/**
	 * Retrieves the accessible menus children of a given parent
	 * 
	 * @param parentId identifier of the parent menu
	 * @param locale currently selected locale
	 * @param enabledOnly to retrieve just the enabled menus
	 * 
	 * @return the accessible children
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public List<GUIMenu> getMenus(long parentId, String locale, boolean enabledOnly) throws ServerException;

	/**
	 * Saves a set of menus
	 * 
	 * @param menus the menus to save
	 * @param locale currently selected locale
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void saveMenus(List<GUIMenu> menus, String locale) throws ServerException;

	/**
	 * Saves a menu
	 * 
	 * @param menu the menu to save
	 * @param locale currently selected locale
	 * 
	 * @return the saved menu
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public GUIMenu saveMenu(GUIMenu menu, String locale) throws ServerException;

	/**
	 * Deletes a menu but only if is not a legacy menu (type not 0)
	 * 
	 * @param menuId the menu to delete
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void deleteMenu(long menuId) throws ServerException;

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
	public List<GUIUser> searchUsers(String username, String groupId) throws ServerException;

	/**
	 * Retrieves the list of actually blocked usernames and IPs detected as
	 * Brute Force Attack
	 * 
	 * @return the array of blocked usernames and IPs
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public List<GUISequence> loadBlockedEntities() throws ServerException;

	/**
	 * Removes blocked entries detected as Brute Force Attack
	 * 
	 * @param id identifiers of entities from the BFA list
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void removeBlockedEntities(List<Long> id) throws ServerException;

	/**
	 * Permanently trusts the current device for the current user
	 * 
	 * @param label optional label to assign to the current device
	 * 
	 * @return the ID of the trusted device
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public String trustDevice(String label) throws ServerException;

	/**
	 * Updates the label of a device
	 * 
	 * @param deviceId identifier of the device to update
	 * @param label label to assign to the current device
	 * 
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void updateDeviceLabel(long deviceId, String label) throws ServerException;

	/**
	 * Check if the saved device ID is trusted for the current user
	 * 
	 * @param device identifier of the device
	 * 
	 * @return if the device is trusted or not
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public Boolean isTrustedDevice(String device) throws ServerException;

	/**
	 * Deletes a set of trusted devices for the current user
	 * 
	 * @param deviceIds identifiers of the devices to delete
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public void deleteTrustedDevices(List<String> deviceIds) throws ServerException;

	/**
	 * Downloads the most recent version of the Geolocation database
	 * 
	 * @param key the API key
	 * 
	 * @return the current database version
	 * 
	 * @throws ServerException error generated in the server application
	 */
	public String syncGeolocationDB(String key) throws ServerException;

	/**
	 * Saves an uploaded image as the user's avatar
	 * 
	 * @param userId Identifier of the user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	void saveAvatar(long userId) throws ServerException;

	/**
	 * Resets tha avatar to the default one
	 * 
	 * @param userId Identifier of the user
	 * 
	 * @throws ServerException error generated in the server application
	 */
	void resetAvatar(long userId) throws ServerException;

	/**
	 * Clones a work time to a set of other users
	 * 
	 * @param srcUserId identifier of the user with the work time you want to
	 *        clone
	 * @param userIds direct ids of users to clone the working time to
	 * @param groupIds the groups of users to clone the working time to
	 * 
	 * @throws ServerException generic error
	 */
	void cloneWorkTimes(long srcUserId, List<Long> userIds, List<Long> groupIds) throws ServerException;

	public static class Instance {
		private static SecurityServiceAsync inst;

		private Instance() {
		}

		public static SecurityServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SecurityService.class);
			}
			return inst;
		}
	}
}