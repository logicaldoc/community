package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;

/**
 * Folder Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 * 
 */
@WebService(name = "Folder", serviceName = "Folder", targetNamespace = "http://ws.logicaldoc.com")
@WSDoc(description = "folders handling and CRUD operations")
public interface FolderService {
	/**
	 * Creates a new folder. The user can completely customize the folder
	 * through a value object containing the folder's metadata
	 * 
	 * @param sid Session identifier
	 * @param folder value object containing the folder's metadata
	 * 
	 * @return The value object containing the folder's metadata
	 * 
	 * @throws Exception error during folder creation
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "Creates a new folder; returns the newly created folder")
	public WSFolder create(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "definition of the new folder")
	@WebParam(name = "folder")
	WSFolder folder) throws Exception;

	/**
	 * Creates a new folder alias
	 * 
	 * @param sid Session identifier
	 * @param parentId The parent folder
	 * @param foldRef The referenced folder
	 * 
	 * @return The newly created alias
	 * 
	 * @throws Exception error during alias creation
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "creates a new folder alias; returns the newly created alias")
	public WSFolder createAlias(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the folder in which the alias will be created")
	long parentId, @WSDoc(description = "identifier of the referenced folder")
	long foldRef) throws Exception;

	/**
	 * Create a new folder.
	 * 
	 * @param sid Session identifier
	 * @param parentId The parent's ID
	 * @param name The new folder's name
	 * @return The newly created folder ID
	 * 
	 * @throws Exception error during folder creation
	 */
	@WebMethod
	@WebResult(name = "folderId")
	@WSDoc(description = "creates a new folder; returns the newly created folder")
	public long createFolder(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "parentId")
	long parentId, @WebParam(name = "name")
	String name) throws Exception;

	/**
	 * Deletes an existing folder.
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "deletes an existing folder")
	public void delete(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Renames an existing folder.
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * @param name The new folder name
	 * 
	 * @throws Exception error during folder rename
	 */
	@WebMethod
	@WSDoc(description = "renames an existing folder")
	public void rename(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WSDoc(description = "the new folder's name")
	@WebParam(name = "name")
	String name) throws Exception;

	/**
	 * Updates an existing folder. To perform this you need the RENAME
	 * permission.
	 * 
	 * @param sid Session identifier
	 * @param folder The folders metadata(please compile the ID)
	 * 
	 * @throws Exception error during update
	 */
	@WebMethod
	@WSDoc(description = "updates an existing folder; you need the RENAME permission")
	public void update(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folder")
	WSFolder folder) throws Exception;

	/**
	 * Moves an existing folder with the given identifier.
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * @param parentId The folder id of the new parent folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "moves an existing folder with the given identifier")
	public void move(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WSDoc(description = "the new folder's parent")
	@WebParam(name = "parentId")
	long parentId) throws Exception;

	/**
	 * Merges the contents of folder into a target
	 * 
	 * @param sid Session identifier
	 * @param sourceId Identifier of the source folder
	 * @param targetId Identifier of the target folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "merges the contents of folder into a target")
	public void merge(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "sourceId")
	long sourceId, @WebParam(name = "targetId")
	long targetId) throws Exception;

	/**
	 * Copies an existing folder with the given identifier.
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * @param targetId The folder id of the target folder
	 * @param foldersOnly If 1, only the folders will be copied and not the
	 *        documents
	 * @param securityOption How to assign the security policies to the newly
	 *        created folders:
	 *        <ul>
	 *        <li><b>null</b> or <b>none</b>: empty security policies</li>
	 *        <li><b>inherit</b>: the new folder will point to the parent for
	 *        the security policies</li>
	 *        <li><b>replicate</b>: the new folder will have a copy of the
	 *        security policies of the source folder</li>
	 *        </ul>
	 * 
	 * @throws Exception error during copy
	 */
	@WebMethod
	@WSDoc(description = "copies an existing folder into another location")
	public void copy(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WSDoc(description = "the new folder's parent")
	@WebParam(name = "targetId")
	long targetId, @WSDoc(description = "<b>0</b> = copy folders and files, <b>1</b> = copy just folders")
	@WebParam(name = "foldersOnly")
	int foldersOnly,
			@WSDoc(description = "<b>null</b> or </b>none</b> = no sec. policies are created, <b>inherit</b>: the new folder will point to the parent for the security policies, <b>replicate</b> = sec. policies are inherited from the new parent folder")
			@WebParam(name = "securityOption")
			String securityOption) throws Exception;

	/**
	 * Gets an existing folder
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * 
	 * @return A value object containing the folder's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "gets an existing folder")
	public WSFolder getFolder(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Gets root metadata
	 * 
	 * @param sid Session identifier
	 * 
	 * @return A value object containing the folder's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "gets the root folder")
	public WSFolder getRootFolder(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws Exception;

	/**
	 * Gets the Default workspace
	 * 
	 * @param sid Session identifier
	 * @return A value object containing the workspace's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "workspace")
	@WSDoc(description = "gets the default workspace")
	public WSFolder getDefaultWorkspace(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws Exception;

	/**
	 * Lists all direct folders of a parent folder.<br>
	 * Attention: readable only sub-folders are returned.
	 * 
	 * @param sid identifier of th session
	 * @param folderId identifier of the folder
	 * 
	 * @return Array of folders contained in the folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "lists all direct children of a parent folder: readable only sub-folders are returned")
	public WSFolder[] listChildren(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Tests if a folder is readable.
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * 
	 * @return True if the identifier denotes a readable folder, otherwise
	 *         false.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "tests if a folder is readable")
	public boolean isReadable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Tests if a folder is writable
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * @return True if the identifier denotes a writable folder, otherwise false
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "tests if a folder is writable")
	public boolean isWritable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Tests if the current user has a specific permission on a folder
	 * 
	 * @param sid Session identifier
	 * @param folderId The folder id
	 * @param permission The permission representation
	 * 
	 * @return True if the identifier denotes a granted permission, otherwise
	 *         false
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "tests if the current user has a specific permission on a folder")
	public boolean isGranted(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WSDoc(description = "the permissions' integer representation")
	@WebParam(name = "permission")
	int permission) throws Exception;

	/**
	 * Computes the path from the root to the target folder.
	 * 
	 * @param sid Session identifier
	 * @param folderId The target folder id
	 * @return The list of folder, the first is the root, the last is the target
	 *         folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folders")
	@WSDoc(description = "computes the path from the root to the target folder; returns the array of folders, the first is the root")
	public WSFolder[] getPath(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Grants user permission to the folder.
	 * 
	 * @param sid Session identifier
	 * @param folderId Folder id
	 * @param userId User Id
	 * @param permissions the permission integer representation. If '0', the
	 *        user will be not granted to access the folder.
	 * @param recursive recursion option. If true, the grant operation is
	 *        applied also to the subfolders.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "grants user permission to the folder")
	public void grantUser(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WebParam(name = "userId")
	long userId,
			@WSDoc(description = "the permission integer representation; if '0', the user will be not granted to access the folder")
			@WebParam(name = "permissions")
			int permissions, @WSDoc(description = "the grant operation is applied also to the subfolders")
			@WebParam(name = "recursive")
			boolean recursive) throws Exception;

	/**
	 * Grants group permission to the folder
	 * 
	 * @param sid Session identifier
	 * @param folderId Folder id
	 * @param groupId Group Id
	 * @param permissions the permission integer representation. If '0', the
	 *        group will be not granted to access the folder.
	 * @param recursive recursion option. If true, the grant operation is
	 *        applied also to the subfolders
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "grants group permission to the folder")
	public void grantGroup(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WebParam(name = "groupId")
	long groupId,
			@WSDoc(description = "the permission integer representation; if '0', the group will be not granted to access the folder")
			@WebParam(name = "permissions")
			int permissions, @WSDoc(description = "the grant operation is applied also to the subfolders")
			@WebParam(name = "recursive")
			boolean recursive) throws Exception;

	/**
	 * Retrieves the list of granted users for the given folder.
	 * 
	 * @param sid Session identifier
	 * @param folderId Folder id
	 * 
	 * @return 'error' if error occurred, the right objects collection.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "retrieves the list of granted users for the given folder")
	public WSRight[] getGrantedUsers(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Retrieves the list of granted groups for the given folder
	 * 
	 * @param sid Session identifier
	 * @param folderId Folder id
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "retrieves the list of granted groups for the given folder")
	public WSRight[] getGrantedGroups(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws Exception;

	/**
	 * Creates the folder for the specified path. All unexisting nodes specified
	 * in the path will be created
	 * 
	 * @param sid Session identifier
	 * @param parentId The parent folder
	 * @param path The folder's path(for example /Default/dog/cat/mouse)
	 * 
	 * @return The created folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "creates the folder for the specified path; all unexisting nodes will be created")
	public WSFolder createPath(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "parentId")
	long parentId, @WSDoc(description = "the folder's path(for example /Default/dog/cat/mouse)")
	@WebParam(name = "path")
	String path) throws Exception;

	/**
	 * Finds the folder at the specified path
	 * 
	 * @param sid Session identifier
	 * @param path The folder's path(for example /Default/dog/cat/mouse)
	 * 
	 * @return The created folder
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "finds the folder at the specified path")
	public WSFolder findByPath(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "the folder's path(for example /Default/dog/cat/mouse)")
	@WebParam(name = "path")
	String path) throws Exception;

	/**
	 * Retrieves the list of all workspaces.
	 * 
	 * @param sid Session identifier
	 * 
	 * @return the list of all workspaces
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WebResult(name = "workspaces")
	@WSDoc(description = "retrieves the list of all workspaces")
	public WSFolder[] listWorkspaces(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws Exception;
}