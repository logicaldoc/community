package com.logicaldoc.webservice.rest;

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

import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;

@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface FolderService {

	@POST
	@Path("/create")
	// The "folder" parameter comes in the POST request body (encoded as XML or JSON).
	public WSFolder create(WSFolder folder) throws Exception;

	@POST
	@Path("/createSimple")
	@Consumes({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
	// The "folderPath" string comes in the POST request body.
	public WSFolder createSimple(String folderPath) throws Exception;

	@POST
	@Path("/createSimpleForm")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	// The "folderPath" parameter comes in the POST as a field
	public WSFolder createSimpleForm(@FormParam("folderPath") String folderPath) throws Exception;

	@POST
	@Path("/createSimpleJSON")
	@Consumes({ MediaType.APPLICATION_JSON })
	// The "folderPath" parameter comes in the POST request body.
	public WSFolder createSimpleJSON(String jsonstr) throws Exception;

	// The parameters come in the POST as fields
	@POST
	@Path("/createPath")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	public WSFolder createPath(@FormParam("parentId") long parentId, @FormParam("path") String path) throws Exception;

	/**
	 * Finds the folder at the specified path
	 * 
	 * @param path the path
	 * 
	 * @return the folder
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/findByPath")
	@Produces({ MediaType.APPLICATION_JSON })
	public WSFolder findByPath(@QueryParam("path") String path) throws Exception;

	/**
	 * Gets the root folder
	 * 
	 * @return the root folder
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getRootFolder")
	@Produces({ MediaType.APPLICATION_JSON })
	public WSFolder getRootFolder() throws Exception;

	@POST
	@Path("/createFolder")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	// The parameters come in the POST request body.
	public long createFolder(@FormParam("parentId") long parentId, @FormParam("name") String name) throws Exception;

	@GET
	@Path("/getFolder")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	public WSFolder getFolder(@QueryParam("folderId") long folderId) throws Exception;

	@DELETE
	@Path("/delete")
	public void delete(@QueryParam("folderId") long folderId) throws Exception;

	@GET
	@Path("/listChildren")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	public WSFolder[] listChildren(@QueryParam("folderId") long folderId) throws Exception;

	@GET
	@Path("/getPath")
	public WSFolder[] getPath(@QueryParam("folderId") long folderId) throws Exception;

	@GET
	@Path("/getPathString")
	public String getPathString(@QueryParam("folderId") long folderId) throws Exception;

	/**
	 * Updates an existing folder. To perform this you need the RENAME permission.
	 * 
	 * @param folder The folders metadata(please compile the ID)
	 * 
	 * @throws Exception error during update
	 */
	@PUT
	@Path("/update")
	public void update(WSFolder folder) throws Exception;

	/**
	 * Renames an existing folder.
	 * 
	 * @param folderId The folder id
	 * @param name The new folder name
	 * 
	 * @throws Exception error during folder rename
	 */
	@PUT
	@Path("/rename")
	public void rename(@QueryParam("folderId") long folderId, @QueryParam("name") String name) throws Exception;

	/**
	 * Moves an existing folder with the given identifier.
	 * 
	 * @param folderId The folder id
	 * @param parentId The folder id of the new parent folder
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/move")
	public void move(@QueryParam("folderId") long folderId, @QueryParam("parentId") long parentId) throws Exception;
	
	
	/**
	 * Creates a new folder alias
	 * 
	 * @param parentId The parent folder
	 * @param foldRef The referenced folder
	 * 
	 * @return The newly created alias
	 * 
	 * @throws Exception error during alias creation
	 */
	@POST
	@Path("/createAlias")
	public WSFolder createAlias(@FormParam("parentId") long parentId, @FormParam("foldRef") long foldRef) throws Exception;
	
	/**
	 * Gets the Default workspace
	 * 
	 * @return A value object containing the workspace's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getDefaultWorkspace")	
	public WSFolder getDefaultWorkspace() throws Exception;
	
	/**
	 * Retrieves the list of all workspaces.
	 * 
	 * @return the list of all workspaces
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/listWorkspaces")	
	public WSFolder[] listWorkspaces() throws Exception;
	
	/**
	 * Tests if a folder is readable.
	 * 
	 * @param folderId The folder id
	 * 
	 * @return True if the identifier denotes a readable folder, otherwise false.
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/isReadable")
	public boolean isReadable(@QueryParam("folderId") long folderId) throws Exception;
	
	/**
	 * Tests if a folder is writable
	 * 
	 * @param folderId The folder id
	 * @return True if the identifier denotes a writable folder, otherwise false
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/isWritable")
	public boolean isWritable(@QueryParam("folderId") long folderId) throws Exception;
	
	/**
	 * Tests if the current user has a specific permission on a folder
	 * 
	 * @param folderId The folder id
	 * @param permission The permission representation
	 * 
	 * @return True if the identifier denotes a granted permission, otherwise false
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/isGranted")
	public boolean isGranted(@QueryParam("folderId") long folderId, @QueryParam("permission") int permission) throws Exception;
	
	/**
	 * Copies an existing folder with the given identifier.
	 * 
	 * @param folderId The folder id
	 * @param targetId The folder id of the target folder
	 * @param foldersOnly If 1, only the folders will be copied and not the documents
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
	@POST
	@Path("/copy")
	public void copy(@FormParam("folderId") long folderId, @FormParam("targetId") long targetId, @FormParam("foldersOnly") int foldersOnly, @FormParam("securityOption") String securityOption) throws Exception;
	
	
	/**
	 * Grants user permission to the folder.
	 * 
	 * @param folderId Folder id
	 * @param userId User Id
	 * @param permissions the permission integer representation. If '0', the user will be not granted to access the folder.
	 * @param recursive recursion option. If true, the grant operation is applied also to the subfolders.
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/grantUser")
	public void grantUser(
			@QueryParam("folderId") long folderId, 
			@QueryParam("userId") long userId,
			@QueryParam("permissions") int permissions,
			@QueryParam("recursive") boolean recursive)
			throws Exception;
	
	
	/**
	 * Grants group permission to the folder
	 * 
	 * @param folderId Folder id
	 * @param groupId Group Id
	 * @param permissions the permission integer representation. If '0', the
	 *        group will be not granted to access the folder.
	 * @param recursive recursion option. If true, the grant operation is
	 *        applied also to the subfolders
	 * 
	 * @throws Exception error in the server application
	 */
	@PUT
	@Path("/grantGroup")
	public void grantGroup(
			@QueryParam("folderId") long folderId, 
			@QueryParam("groupId") long groupId,
			@QueryParam("permissions") int permissions,
			@QueryParam("recursive") boolean recursive)
			throws Exception;
	
	/**
	 * Retrieves the list of granted groups for the given folder
	 * 
	 * @param folderId Folder id
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getGrantedGroups")
	public WSRight[] getGrantedGroups(@QueryParam("folderId") long folderId) throws Exception;
	
	/**
	 * Retrieves the list of granted users for the given folder.
	 * 
	 * @param folderId Folder id
	 * 
	 * @return 'error' if error occurred, the right objects collection.
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getGrantedUsers")
	public WSRight[] getGrantedUsers(@QueryParam("folderId") long folderId) throws Exception;	
}