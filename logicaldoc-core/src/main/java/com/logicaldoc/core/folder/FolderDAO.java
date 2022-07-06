package com.logicaldoc.core.folder;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Permission;

/**
 * Instances of this class is a DAO-service for folder objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 6.0
 */
public interface FolderDAO extends PersistentObjectDAO<Folder> {

	/**
	 * Gets a folder by a given ID if it is an alias, the referenced folder is
	 * returned.
	 * 
	 * @param folderId The ID
	 * 
	 * @return A real folder that is referenced by the given ID
	 */
	public Folder findFolder(long folderId);

	/**
	 * Finds all folders by folder name
	 * 
	 * @param name name of the folder
	 * @param tenantId Specification of the owning tenant (optional).
	 * 
	 * @return List of folders with given folder name.
	 */
	public List<Folder> findByName(String name, Long tenantId);

	/**
	 * Finds all folders by folder text, contained in the parent folder.
	 * 
	 * @param parent The parent folder(optional)
	 * @param name The folder name to search for
	 * @param tenantId Specification of the owning tenant (optional). If not
	 *        specified the tenant of parent is used instead
	 * @param caseSensitive true if the search must be case sensitive
	 * 
	 * @return List of folders with given name
	 */
	public List<Folder> findByName(Folder parent, String name, Long tenantId, boolean caseSensitive);

	/**
	 * Retrieves the root folder of the given tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return the root folder
	 */
	public Folder findRoot(long tenantId);

	/**
	 * Retrieves the Default workspace of the given tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * 
	 * @return The default workspace
	 */
	public Folder findDefaultWorkspace(long tenantId);

	/**
	 * Finds authorized folders for a user
	 * 
	 * @param userId ID of the user
	 * 
	 * @return List of found folders
	 */
	public List<Folder> findByUserId(long userId);

	/**
	 * Finds authorized folders for a user having a specified tag.
	 * 
	 * @param userId ID of the user
	 * @param tag Tag of the folder
	 * @param max Optional, defines the maximum records number
	 * 
	 * @return Collection of found folders
	 */
	public List<Folder> findByUserIdAndTag(long userId, String tag, Integer max);

	/**
	 * Finds all folders ids with a specific permission enabled on the specifies
	 * user
	 * 
	 * @param userId The user identifier
	 * @param permission The permission to check
	 * @param parentId The id of the parent folder to inspect (optional)
	 * @param tree If true, the parentId will be interpreted as the root of a
	 *        tree
	 * 
	 * @return List of selected folder IDs.
	 */
	public Collection<Long> findFolderIdByUserIdAndPermission(long userId, Permission permission, Long parentId,
			boolean tree);

	/**
	 * This method selects only the folder ID from the folders for which a user
	 * is authorized.
	 * 
	 * @param userId ID of the user.
	 * @param parentId The id of the parent folder to inspect (optional)
	 * @param tree If true, the parentId will be interpreted as the root of a
	 *        tree
	 * 
	 * @return List of selected folder IDs.
	 */
	public Collection<Long> findFolderIdByUserId(long userId, Long parentId, boolean tree);

	/**
	 * Retrieve all the ids of the folders in a given tree
	 * 
	 * @param rootId Root of the tree
	 * @param includeDeleted True if the deleted records need to be loaded
	 * 
	 * @return List of selected folder IDs, rootId is included as well
	 */
	public Collection<Long> findFolderIdInTree(long rootId, boolean includeDeleted);

	/**
	 * Retrieve all the ids of the folders in a given tree using the path
	 * attribute
	 * 
	 * @param rootId Root of the folder
	 * @param includeDeleted True if the deleted records need to be loaded
	 * 
	 * @return List of selected folder IDs, rootId is included as well
	 */
	public Collection<Long> findFolderIdInPath(long rootId, boolean includeDeleted);

	/**
	 * This method selects only the folder ID from the folders for which a user
	 * is authorized.
	 * 
	 * @param userId ID of the user.
	 * @param parentId The id of the parent folder to inspect (optional)
	 * 
	 * @return List of selected folder IDs.
	 */
	public Collection<Long> findFolderIdByUserIdInPath(long userId, Long parentId);

	/**
	 * Finds direct children of a folder.
	 * 
	 * @param parentId ID of the folder which children are wanted
	 * @param userId identifier of the user
	 * 
	 * @return List of found folders sorted by text
	 */
	public List<Folder> findByUserId(long userId, long parentId);

	/**
	 * Finds all children(direct and indirect) by parentId
	 * 
	 * @param parentId identifier of the parent folder
	 * 
	 * @return List of found folders
	 */
	public List<Folder> findByParentId(long parentId);

	/**
	 * Finds all children(direct and indirect) by parentId
	 * 
	 * @param parentId identifier of the parent folder
	 * 
	 * @return list of folder IDs
	 */
	public List<Long> findIdsByParentId(long parentId);

	/**
	 * Finds direct children of a folder
	 * 
	 * @param parentId Folder ID of the folder which children are wanted
	 * @param max Optional, maximum number of children
	 * 
	 * @return List of found folders
	 */
	public List<Folder> findChildren(long parentId, Integer max);

	/**
	 * Finds direct children of a folder accessible by the given user
	 * 
	 * @param parentId Folder ID of the folder which children are wanted
	 * @param userId Identifier of the user that must have read access
	 * 
	 * @return List of found folders
	 */
	public List<Folder> findChildren(long parentId, long userId);

	/**
	 * This method is looking up for writing rights for a folder and an user.
	 * 
	 * @param folderId ID of the folder
	 * @param userId ID of the user
	 * 
	 * @return if the write permission is granted
	 */
	public boolean isWriteEnabled(long folderId, long userId);

	public boolean isReadEnabled(long folderId, long userId);

	public boolean isPrintEnabled(long folderId, long userId);

	public boolean isDownloadEnabled(long folderId, long userId);

	public boolean isMoveEnabled(long folderId, long userId);

	/**
	 * This method checks if the given permission is enabled for a folder and an
	 * user.
	 * 
	 * @param permission the permission to check
	 * @param folderId ID of the folder
	 * @param userId ID of the user
	 * 
	 * @return if the permission is granted to the user
	 */
	public boolean isPermissionEnabled(Permission permission, long folderId, long userId);

	/**
	 * Finds all permissions of a user enabled on the specified folder
	 * 
	 * @param folderId ID of the folder
	 * @param userId ID of the user
	 * 
	 * @return Collection of enabled permissions
	 */
	public Set<Permission> getEnabledPermissions(long folderId, long userId);

	/**
	 * This method selects only the folder ID from the folders for which a user
	 * is authorized. Only folders direct child of the specified parent are
	 * returned.
	 * 
	 * @param userId ID of the user
	 * @param parentId Parent folder
	 * @return List of selected folder ID's.
	 */
	public List<Long> findIdByUserId(long userId, long parentId);

	/**
	 * Checks that the user has access to the folder and all its sub-items
	 * 
	 * @param folder the folder
	 * @param userId identifier of the document
	 * 
	 * @return if the user has write permission
	 */
	public boolean hasWriteAccess(Folder folder, long userId);

	/**
	 * Finds all folders accessible by the passed group
	 * 
	 * @param groupId The group id
	 * 
	 * @return The List of folders
	 */
	public List<Folder> findByGroupId(long groupId);

	/**
	 * Returns a List of folders being a parent of the given folder. The list is
	 * ordered starting from the root of folders. The list doesn't include the
	 * given folder.
	 * 
	 * @param id ID of the folder
	 * 
	 * @return the hierarchy of parent folders
	 */
	public List<Folder> findParents(long id);

	/**
	 * Returns the workspace that contains the given folder
	 * 
	 * @param folderId ID of the folder
	 * 
	 * @return the workspace containing the fiven folder
	 */
	public Folder findWorkspace(long folderId);

	/**
	 * Restores a previously deleted folder
	 * 
	 * @param folderId The folder identifier
	 * @param parentId The parent folder to restore in
	 * @param transaction Current session informations
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public void restore(long folderId, long parentId, FolderHistory transaction) throws PersistenceException;

	/**
	 * Finds that folder that lies under a specific parent (given by the id) an
	 * with a given name(like operator is used)
	 * 
	 * @param name name of the dolder
	 * @param parentId identifier of the parent folder
	 * 
	 * @return list of folders
	 */
	public List<Folder> findByNameAndParentId(String name, long parentId);

	/**
	 * Same as store(Folder, boolean, FolderHistory)
	 * 
	 * @param folder the folder to store
	 * @param transaction current session informations
	 * 
	 * @return true if the folder has been correctly stored
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean store(Folder folder, FolderHistory transaction) throws PersistenceException;

	/**
	 * Shortcut for deleteAll(folders, 1, transaction)
	 * 
	 * @param folders the folders
	 * @param transaction current session informations
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public void deleteAll(Collection<Folder> folders, FolderHistory transaction) throws PersistenceException;

	/**
	 * For each folder, save the folder delete history entry for each folder and
	 * delete the folder
	 * 
	 * @param folders List of folder to be delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event on each folder
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public void deleteAll(Collection<Folder> folders, int delCode, FolderHistory transaction)
			throws PersistenceException;

	/**
	 * Shortcut for delete(id, 1, transaction)
	 * 
	 * @param id the folder identifier
	 * @param transaction the session informations
	 * 
	 * @return if the folde has been succesfully deleted
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean delete(long id, FolderHistory transaction) throws PersistenceException;

	/**
	 * This method deletes the folder object and insert a new folder history
	 * entry
	 * 
	 * @param id The id of the folder to delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event
	 * 
	 * @return True if successfully deleted from the database
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean delete(long id, int delCode, FolderHistory transaction) throws PersistenceException;

	/**
	 * Creates a new folder in the parent Folder
	 * 
	 * @param parent The parent folder
	 * @param folderVO The folder's metadata
	 * @param inheritSecurity If true the new folder will 'point' to the parent
	 *        for the security policies.
	 * @param transaction optional transaction entry to log the event
	 * 
	 * @return The newly created folder
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public Folder create(Folder parent, Folder folderVO, boolean inheritSecurity, FolderHistory transaction)
			throws PersistenceException;

	/**
	 * Creates a new folder folder alias
	 * 
	 * @param parentId The parent folder
	 * @param foldRef The referenced folder
	 * @param transaction optional transaction entry to log the event
	 * 
	 * @return The newly created alias
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public Folder createAlias(long parentId, long foldRef, FolderHistory transaction) throws PersistenceException;

	/**
	 * Finds all the aliases
	 * 
	 * @param foldRef The referenced folder
	 * @param tenantId The tenant
	 * 
	 * @return Collection of aliases
	 */
	public List<Folder> findAliases(Long foldRef, long tenantId);

	/**
	 * Creates the folder for the specified path. All unexisting nodes specified
	 * in the path will be created. The path must be in any case inside a
	 * workspace, if not, the Default one will be used.
	 * 
	 * @param parent The parent folder
	 * @param path The folder path(for example /dog/cat/mouse)
	 * @param inheritSecurity If true the new folders will 'point' to the parent
	 *        for the security policies.
	 * @param transaction optional transaction entry to log the event
	 * 
	 * @return The created folder
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public Folder createPath(Folder parent, String path, boolean inheritSecurity, FolderHistory transaction)
			throws PersistenceException;

	/**
	 * Dynamically computes the path for the specified folder. The path is a
	 * sequence of IDS in the form: /4/55897/99870
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return the folder's path
	 */
	public String computePath(long folderId);

	/**
	 * Dynamically computes the path extended for the specified folder. The path
	 * extended is a human readable path in the form: /folder1/folder2/folder3
	 * 
	 * @param folderId the folder's identifier
	 * 
	 * @return The path extended
	 */
	public String computePathExtended(long folderId);

	/**
	 * Retrieval of a folder by the extended path
	 * 
	 * @param pathExtended the path extended
	 * @param tenantId identifier of the tenant
	 * 
	 * @return The folder that matched the given path
	 */
	public Folder findByPathExtended(String pathExtended, long tenantId);

	/**
	 * Moves a folder into another folder
	 * 
	 * @param source The folder to move
	 * @param target The target folder
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public void move(Folder source, Folder target, FolderHistory transaction) throws PersistenceException;

	/**
	 * Copies a folder into another folder
	 * 
	 * @param source The folder to copy
	 * @param target The target folder
	 * @param newName optional new name of the copied folder
	 * @param foldersOnly True if only the folders tree has to be copied; if
	 *        false, the documents will also be copied
	 * @param securityOption How to assign the security policies to the newly created folders:
	 * <ul>
	 * <li><b>null</b> or <b>none</b>: empty security policies</li>
	 * <li><b>inherit</b>: the new folder will point to the parent for the security policies</li>
	 * <li><b>replicate</b>: the new folder will have a copy of the security policies of the source folder</li>
	 * </ul>
	 *        
	 * @param transaction entry to log the event (set the user)
	 * @return The new folder created
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public Folder copy(Folder source, Folder target, String newName, boolean foldersOnly, String securityOption,
			FolderHistory transaction) throws PersistenceException;

	/**
	 * Deletes a folder and all its sub-folders that a user can delete. After
	 * recovering of all sub-folders inside the folder, will be canceled all
	 * folders for which the user has the delete permission or there isn't an
	 * immutable document inside it.<br>
	 * 
	 * <b>Important:</b> Remember to delete orphaned documents.
	 * 
	 * @param folder Folder to delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event (set the user)
	 * @return List of folders that the user cannot delete(permissions, o
	 *         immutable documents presents)
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public List<Folder> deleteTree(Folder folder, int delCode, FolderHistory transaction) throws PersistenceException;

	/**
	 * Delete a folder and all its sub-folders that a user can delete. After
	 * recovering of all sub-folders inside the folder, will be canceled all
	 * folders for which the user has the delete permission or there isn't an
	 * immutable document inside it. <br>
	 * 
	 * <b>Important:</b> All the contained documents will be deleted
	 * 
	 * @param folderId Folder to delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return List of folders that the user cannot delete(permissions, o
	 *         immutable documents presents)
	 * 
	 * @throws Exception in case of any error
	 */
	public List<Folder> deleteTree(long folderId, int delCode, FolderHistory transaction) throws Exception;

	/**
	 * Shortcut for deleteTree(folderId, 1, transaction)
	 * 
	 * @param folderId Folder to delete
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return List of folders that the user cannot delete(permissions, o
	 *         immutable documents presents)
	 * 
	 * @throws Exception in case of any error
	 */
	public List<Folder> deleteTree(long folderId, FolderHistory transaction) throws Exception;

	/**
	 * Useful method that allows to find all folders that contains the given
	 * name into their text.
	 * 
	 * @param name The name to be found
	 * @param tenantId The tenant to search in
	 * 
	 * @return List of folders that contains the given name into their text.
	 */
	public List<Folder> find(String name, Long tenantId);

	/**
	 * Finds all deleted folders of a specific user.
	 * 
	 * @param userId The user that performed the deletion
	 * @param maxHits Optional defines the max number of returned hits
	 * 
	 * @return The folders list
	 */
	public List<Folder> findDeleted(long userId, Integer maxHits);

	/**
	 * Checks if a folder with the given folderId is parent of the folder with
	 * the given targetId
	 * 
	 * @param parentId The folder to be checked
	 * @param childId The target folder
	 * 
	 * @return True if the folder with the given parentId is parent of the
	 *         folder with the given childId
	 */
	public boolean isInPath(long parentId, long childId);

	/**
	 * Propagates the security policies of a node to the whole subtree
	 * 
	 * @param rootId identifier of the root folder
	 * @param transaction session informations
	 * 
	 * @return if the rights have been correctly applied
	 */
	public boolean applyRightToTree(long rootId, FolderHistory transaction);

	/**
	 * Propagates the grid layout of a node to the whole subtree
	 * 
	 * @param rootId identifier of the root folder
	 * @param transaction session informations
	 * 
	 * @return if the grid layout has been correctly replicated
	 */
	public boolean applyGridToTree(long rootId, FolderHistory transaction);

	/**
	 * Changes the securityRef of the given folder, all the other folders that
	 * inherits from this one will be changed accordingly.
	 * 
	 * @param folderId identifier of the folder
	 * @param rightsFolderId id of the folder to inherit from
	 * @param transaction session informations
	 * 
	 * @return is the update has been successful
	 */
	public boolean updateSecurityRef(long folderId, long rightsFolderId, FolderHistory transaction);

	/**
	 * Propagates the template metadata to the whole subree
	 * 
	 * @param id identifier of the folder
	 * @param transaction session informations
	 * 
	 * @return is the update has been successful
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean applyMetadataToTree(long id, FolderHistory transaction) throws PersistenceException;

	/**
	 * Propagates the tags to the whole subree
	 * 
	 * @param id identifier of the folder
	 * @param transaction session informations
	 *
	 * @return is the update has been successful
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean applyTagsToTree(long id, FolderHistory transaction) throws PersistenceException;

	/**
	 * Propagates the storage setting to the whole subree
	 * 
	 * @param id identifier of the folder
	 * @param transaction session informations
	 *
	 * @return is the update has been successful
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean applyStorageToTree(long id, FolderHistory transaction) throws PersistenceException;

	/**
	 * Propagates the OCR settings to the whole subree
	 * 
	 * @param id identifier of the folder
	 * @param transaction session informations
	 *
	 * @return is the update has been successful
	 * 
	 * @throws PersistenceException in case of database error
	 */
	public boolean applyOCRToTree(long id, FolderHistory transaction) throws PersistenceException;

	/**
	 * Counts the number of folders
	 * 
	 * @param computeDeleted if the deleted folders have to be taken int account
	 * 
	 * @return the number of folders
	 */
	public int count(boolean computeDeleted);

	/**
	 * Retrieves all the workspaces in the system, that are the first-level
	 * folders of type {@link Folder#TYPE_WORKSPACE}
	 * 
	 * @param tanantId identifier of the tenant
	 * 
	 * @return list of folders
	 */
	public List<Folder> findWorkspaces(long tanantId);

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param folder The folder to be initialized
	 */
	public void initialize(Folder folder);

	/**
	 * Utility method that logs into the DB the transaction that involved the
	 * passed folder. The transaction must be provided with userId and userName.
	 * 
	 * @param folder the folder
	 * @param transaction the session informations
	 */
	public void saveFolderHistory(Folder folder, FolderHistory transaction);

	/**
	 * Counts the number of documents inside a given folder's tree (direct and
	 * indirect children)
	 * 
	 * @param rootId identifier of the root folder
	 * 
	 * @return the number of documents contained in the tree
	 */
	public long countDocsInTree(long rootId);

	/**
	 * Counts the number of documents inside a given folder's tree (direct and
	 * indirect children)
	 * 
	 * @param rootId identifier of the root folder
	 * 
	 * @return sum of the sizes of the documents contained in the tree expressed
	 *         in bytes
	 */
	public long computeTreeSize(long rootId);

	/**
	 * Retrieves the alphabetically ordered list of all the folder's tags
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return list of tags
	 */
	public List<String> findTags(long folderId);

	/**
	 * Merges the contents of two folders
	 * 
	 * @param source The folder whose contents must be merged inside
	 *        <b>folderA</b>
	 * @param target The main folder to use as container
	 * @param transaction the session informations
	 * 
	 * @throws PersistenceException in case of database or logical error
	 */
	public void merge(Folder source, Folder target, FolderHistory transaction) throws PersistenceException;
}