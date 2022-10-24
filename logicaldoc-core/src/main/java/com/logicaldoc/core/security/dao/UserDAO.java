package com.logicaldoc.core.security.dao;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserHistory;

/**
 * This class is a DAO-service for User-objects.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 * @version 1.0
 */
public interface UserDAO extends PersistentObjectDAO<User> {

	/**
	 * Counts the total number of standard users
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return total number of guest users
	 */
	public int count(Long tenantId);

	/**
	 * Counts the total number of guest users
	 *
	 * @param tenantId identifier of the tenant
	 * 
	 * @return total number of guest users
	 */
	public int countGuests(Long tenantId);

	/**
	 * This method finds an User by its username. The search is case sensitive.
	 * 
	 * @param username username of wanted User.
	 * 
	 * @return Wanted User or null if user doesn't exist.
	 */
	public User findByUsername(String username);

	/**
	 * This method finds an User by its username. The search is case
	 * insensitive.
	 * 
	 * @param username username of wanted User.
	 * 
	 * @return Wanted User or null if user doesn't exist.
	 */
	public User findByUsernameIgnoreCase(String username);

	/**
	 * Depending on the global setting, it invokes findByUsername or
	 * findByUsernameIgnoreCase
	 * 
	 * @param username The username of the user
	 * 
	 * @return The found user
	 */
	public User getUser(String username);

	/**
	 * This method finds an User by username.
	 * 
	 * @param username The username of wanted User.
	 * 
	 * @return Collection of selected users.
	 */
	public List<User> findByLikeUsername(String username);

	/**
	 * This method finds an User by name.
	 * 
	 * @param name The name of wanted User.
	 * 
	 * @return Collection of selected users.
	 */
	public List<User> findByName(String name);

	/**
	 * This method finds an User by username and name.
	 * 
	 * @param username The username of wanted user
	 * @param name The name of wanted user
	 * 
	 * @return Collection of selected users
	 */
	public List<User> findByUsernameAndName(String username, String name);

	/**
	 * Validates an username and a password
	 * 
	 * @param username Username of the User to be validated
	 * @param password Password of the User to be validated
	 * 
	 * @return True if User is valid and authenticated.
	 */
	public boolean validateUser(String username, String password);

	/**
	 * Validates an username only (the password content is not inspected).
	 * 
	 * @param username Username of the User to be validated.
	 * 
	 * @return True if User is valid.
	 */
	public boolean validateUser(String username);

	/**
	 * Is password expired.
	 * 
	 * @param username Username of the User to be validated.
	 * 
	 * @return True if the password is expired
	 */
	public boolean isPasswordExpired(String username);

	/**
	 * Checks if a user is inactive, that is a user without interactions in the
	 * last X days after it has last enabled. We look at the
	 * {@link User#getMaxInactivity()} first en then we fallback to the general
	 * parameter security.user.maxinactivity
	 * 
	 * @param username identifier of the user
	 * 
	 * @return the last interaction time
	 */
	public boolean isInactive(String username);

	/**
	 * This method deletes the user object and insert a new user history entry.
	 * 
	 * @param userId The id of the user to delete
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at database level
	 */
	public void delete(long userId, UserHistory transaction) throws PersistenceException;

	/**
	 * This method persists the user object and insert a new user history entry.
	 * 
	 * @param user the user to store
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at database level
	 */
	public void store(final User user, final UserHistory transaction) throws PersistenceException;

	/**
	 * Retrieves the settings for a user. The settings are stored as Generics of
	 * type <b>usersetting</b>.
	 * 
	 * @param userId Identifier of the user
	 * @param namePrefix Name prefix of the property (optional)
	 * 
	 * @return The map setting_name-generic
	 */
	public Map<String, Generic> findUserSettings(long userId, String namePrefix);

	/**
	 * Retrieve the administrator for the given tenant. The general rule is that
	 * the administrator's username is:
	 * <ol>
	 * <li>admin if the tenant is default</li>
	 * <li>admin<b>Tenantname</b> in all other cases</li>
	 * </ol>
	 * 
	 * @param tenantName name of the tenant
	 * 
	 * @return the user to administrate the tenant
	 */
	public User findAdminUser(String tenantName);

	/**
	 * Retrieves the users belonging to a given group.
	 * 
	 * @param groupId Identifier of the group
	 * 
	 * @return the set of groups
	 */
	public Set<User> findByGroup(long groupId);
}