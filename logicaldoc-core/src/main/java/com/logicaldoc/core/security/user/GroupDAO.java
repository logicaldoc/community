package com.logicaldoc.core.security.user;

import java.util.Collection;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

public interface GroupDAO extends PersistentObjectDAO<Group> {
	
	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static GroupDAO get() {
		return Context.get(GroupDAO.class);
	}
	
	/**
	 * This method persists a new group object. All permissions and extended
	 * attributes of the parent group will be replicated.
	 * 
	 * @param group Group which should be stored in a database.
	 * @param parentGroupId ID of the group this group inherits ACLs from
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void insert(Group group, long parentGroupId) throws PersistenceException;

	/**
	 * This method replicates all ACLs of the parent group to another group.
	 * <p>
	 * <b>Attention:</b> The group(groupId) ACLs will be discarded.
	 * 
	 * @param group The group to be altered.
	 * @param parentGroupId The group whose ACLs will be inherited.
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void inheritACLs(Group group, long parentGroupId) throws PersistenceException;

	/**
	 * Finds a group by name.
	 * 
	 * @param name name of wanted group.
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return Wanted group or null.
	 * @throws PersistenceException Error in the database
	 */
	public Group findByName(String name, long tenantId) throws PersistenceException;

	/**
	 * This method selects all group names
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return collection of all the group names
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Collection<String> findAllGroupNames(long tenantId) throws PersistenceException;

	/**
	 * This method finds a Group by name.
	 * 
	 * @param name The name of wanted Group.
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return Collection of selected groups.
	 * @throws PersistenceException Error in the database
	 */
	public Collection<Group> findByLikeName(String name, long tenantId) throws PersistenceException;

	/**
	 * Counts the total number of groups
	 * 
	 * @return total number of groups
	 * @throws PersistenceException Error in the database
	 */
	public int count() throws PersistenceException;

	/**
	 * Initializes the group collections
	 */
	public void initialize(Group group);

	public void fixGuestPermissions(Group group) throws PersistenceException;
}