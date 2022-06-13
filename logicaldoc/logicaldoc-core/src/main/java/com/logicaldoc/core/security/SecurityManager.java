package com.logicaldoc.core.security;

import java.util.Set;

/**
 * Manager for security objects like users and groups
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public interface SecurityManager {

	/**
	 * Retrieves the collection of groups that can access the given menu
	 * 
	 * @param menuId The menu to consider
	 * @return The collection of allowed groups
	 */
	public Set<Group> getAllowedGroups(long menuId);

	/**
	 * Checks if a given user is member of a particular group
	 * 
	 * @param userId The user identifier
	 * @param groupId The group identifier
	 * @return true only if the user belongs to the group
	 */
	public boolean isMemberOf(long userId, long groupId);

	/**
	 * Checks if a given user is member of a particular group
	 * 
	 * @param userId The user identifier
	 * @param groupName The group name
	 * @return true only if the user belongs to the group
	 */
	public boolean isMemberOf(long userId, String groupName);

	/**
	 * This method is looking up for writing rights for a folder and an user.
	 * 
	 * @param docId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return if the user has the write permission on the document
	 */
	public boolean isWriteEnabled(long docId, long userId);

	public boolean isReadEnabled(long docId, long userId);

	public boolean isPrintEnabled(long docId, long userId);

	public boolean isDownloadEnabled(long docId, long userId);

	/**
	 * This method checks if the given permission is enabled for a document and
	 * an user.
	 * 
	 * @param permission the permission to check
	 * @param docId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return if the permission is granted to the user on the document
	 */
	public boolean isPermissionEnabled(Permission permission, long docId, long userId);

	/**
	 * Finds all permissions of a user enabled on the specified document
	 * 
	 * @param docId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return Collection of permissions granted to the user on the document
	 */
	public Set<Permission> getEnabledPermissions(long docId, long userId);
}