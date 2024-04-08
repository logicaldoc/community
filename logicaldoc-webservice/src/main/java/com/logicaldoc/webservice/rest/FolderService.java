package com.logicaldoc.webservice.rest;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSFolder;

@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface FolderService {

	@POST
	@Path("/create")
	public WSFolder create(WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@POST
	@Path("/createSimple")
	@Consumes({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
	public WSFolder createSimple(String folderPath)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@POST
	@Path("/createSimpleForm")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public WSFolder createSimpleForm(@FormParam("folderPath")
	String folderPath) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@POST
	@Path("/createSimpleJSON")
	@Consumes({ MediaType.APPLICATION_JSON })
	public WSFolder createSimpleJSON(String jsonstr)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@POST
	@Path("/createPath")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public WSFolder createPath(@FormParam("parentId")
	long parentId, @FormParam("path")
	String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Finds the folder at the specified path
	 * 
	 * @param path the path
	 * 
	 * @return the folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/findByPath")
	@Produces({ MediaType.APPLICATION_JSON })
	public WSFolder findByPath(@QueryParam("path")
	String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets the root folder
	 * 
	 * @return the root folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getRootFolder")
	@Produces({ MediaType.APPLICATION_JSON })
	public WSFolder getRootFolder() throws AuthenticationException, WebserviceException, PersistenceException;

	@POST
	@Path("/createFolder")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	public long createFolder(@FormParam("parentId")
	long parentId, @FormParam("name")
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@GET
	@Path("/getFolder")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	public WSFolder getFolder(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@DELETE
	@Path("/delete")
	public void delete(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@GET
	@Path("/listChildren")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	public List<WSFolder> listChildren(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@GET
	@Path("/getPath")
	public List<WSFolder> getPath(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@GET
	@Path("/getPathString")
	public String getPathString(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Updates an existing folder. To perform this you need the RENAME
	 * permission.
	 * 
	 * @param folder The folders metadata(please compile the ID)
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@PUT
	@Path("/update")
	public void update(WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Renames an existing folder.
	 * 
	 * @param folderId The folder id
	 * @param name The new folder name
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@PUT
	@Path("/rename")
	public void rename(@QueryParam("folderId")
	long folderId, @QueryParam("name")
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Moves an existing folder with the given identifier.
	 * 
	 * @param folderId The folder id
	 * @param parentId The folder id of the new parent folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 */
	@PUT
	@Path("/move")
	public void move(@QueryParam("folderId")
	long folderId, @QueryParam("parentId")
	long parentId) throws AuthenticationException, PersistenceException, WebserviceException, PermissionException;

	/**
	 * Creates a new folder alias
	 * 
	 * @param parentId The parent folder
	 * @param foldRef The referenced folder
	 * 
	 * @return The newly created alias
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@POST
	@Path("/createAlias")
	public WSFolder createAlias(@FormParam("parentId")
	long parentId, @FormParam("foldRef")
	long foldRef) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets the Default workspace
	 * 
	 * @return A value object containing the workspace's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/getDefaultWorkspace")
	public WSFolder getDefaultWorkspace()
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the list of all workspaces.
	 * 
	 * @return the list of all workspaces
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/listWorkspaces")
	public List<WSFolder> listWorkspaces() throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a folder is readable.
	 * 
	 * @param folderId The folder id
	 * 
	 * @return True if the identifier denotes a readable folder, otherwise
	 *         false.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isReadable")
	public boolean isReadable(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a folder is writable
	 * 
	 * @param folderId The folder id
	 * 
	 * @return True if the identifier denotes a writable folder, otherwise false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isWritable")
	public boolean isWritable(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if the current user has a specific permission on a folder
	 * 
	 * @param folderId The folder id
	 * @param permission The permission to check (eg: 'read', 'write', ...)
	 * 
	 * @return True if the identifier denotes a granted permission, otherwise
	 *         false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 */
	@GET
	@Path("/isGranted")
	public boolean isGranted(@QueryParam("folderId")
	long folderId, @QueryParam("permission")
	String permission) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Copies an existing folder with the given identifier.
	 * 
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
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException A generic error in the WebService
	 * @throws AuthenticationException Invalid credentials
	 * @throws PermissionException The current user does not have enough
	 *         permissions
	 */
	@POST
	@Path("/copy")
	public void copy(@FormParam("folderId")
	long folderId, @FormParam("targetId")
	long targetId, @FormParam("foldersOnly")
	int foldersOnly, @FormParam("securityOption")
	String securityOption)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	
	/**
	 * Sets the Access Control List
	 * 
	 * @param folderId Folder id
	 * @param acl the complete Access Control List
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@PUT
	@Path("/setAccessControlList")
	public void setAccessControlList(@QueryParam("folderId") long folderId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException;
	
	/**
	 * Retrieves the access control list
	 * 
	 * @param folderId Folder id
	 * 
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getAccessControlList")
	public List<WSAccessControlEntry> getAccessControlList(@QueryParam("folderId") long folderId) 
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;
}