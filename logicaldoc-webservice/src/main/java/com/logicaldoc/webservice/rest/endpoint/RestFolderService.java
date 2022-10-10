package com.logicaldoc.webservice.rest.endpoint;

import java.util.HashMap;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSRight;
import com.logicaldoc.webservice.rest.FolderService;
import com.logicaldoc.webservice.soap.endpoint.SoapFolderService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "folder")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestFolderService extends SoapFolderService implements FolderService {

	private static Logger log = LoggerFactory.getLogger(RestFolderService.class);

	@POST
	@Path("/create")
	@Operation(operationId = "createFolderByModel", summary = "Creates a new folder", description = "The 'folder' metadata comes in the POST request body (encoded as JSON). Note: folder object must specify at least fields name and parentId")
	// @ApiOperation(nickname = "createFolderByModel", value = "Creates a new
	// folder", notes = "The 'folder' metadata comes in the POST request body
	// (encoded as JSON). Note: folder object must specify at least fields name
	// and parentId")
	public WSFolder create(
//			@ApiParam(value = "The folder metadata", required = true, examples = @Example(value = {
//			@ExampleProperty(value = "{ \"parentId\": 4, \"name\": \"Folder created by REST ws\"}") })) 

			@Parameter(description = "The folder metadata", required = true, example = "{ \"parentId\": 4, \"name\": \"Folder created by REST ws\"}", schema = @Schema(implementation = WSFolder.class))
			WSFolder folder) throws Exception {
		log.debug("create()");
		String sid = validateSession();
		return super.create(sid, folder);
	}

	@POST
	@Path("/createSimpleForm")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	// @ApiOperation(value = "Creates folders from path", notes = "Creates
	// folders using an input path. All the folders in the path will be created.
	// It returns the metadata object representing the latest created folder in
	// the path")
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createSimpleForm(
			@Parameter(description = "The string representing the path to be created", example = "/Default/newfolder")
			@FormParam("folderPath")
			String folderPath) throws Exception {
		log.debug("createSimpleForm()");
		String sid = validateSession();
		log.debug("sid: " + sid);
		WSFolder root = super.getRootFolder(sid);
		log.debug("root: " + root);
		log.debug("root: {}", root);
		try {
			return super.createPath(sid, root.getId(), folderPath);
		} catch (RuntimeException re) {
			log.error("createSimpleForm RuntimeException", re);
			throw re;
		} catch (Exception e) {
			log.error("createSimpleForm Exception", e);
			throw e;
		} catch (Throwable tw) {
			log.error("createSimpleForm Throwable", tw);
			throw tw;
		}
	}

	@POST
	@Path("/createSimpleJSON")
	@Consumes({ MediaType.APPLICATION_JSON })
	// @ApiOperation(value = "Creates folders from path", notes = "Creates
	// folders using an input path. All the folders in the path will be created.
	// It returns the metadata object representing the latest created folder in
	// the path")
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createSimpleJSON(
			@Parameter(name = "folderPath", description = "The string representing the path to be created", example = "{\"folderPath\":\"/Default/central/repo\"}")
			String jsonstr) throws Exception {
		log.debug("createSimpleJSON()");

		String sid = validateSession();

		ObjectMapper mapper = new ObjectMapper();
		TypeReference<HashMap<String, String>> typeRef = new TypeReference<HashMap<String, String>>() {
		};
		HashMap<String, String> hm = mapper.readValue(jsonstr, typeRef);

		String folderPath = hm.get("folderPath");

		WSFolder root = super.getRootFolder(sid);
		return super.createPath(sid, root.getId(), folderPath);
	}

	@POST
	@Path("/createSimple")
	@Consumes({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
	// @ApiOperation(value = "Creates folders from path", notes = "Creates
	// folders using an input path. All the folders in the path will be created.
	// It returns the metadata object representing the latest created folder in
	// the path. "
//			+ "Example: curl -u admin:admin -X POST -H ''Content-Type: text/plain'' -H ''Accept: application/json'' -d ''/Default/Curl/newfolder'' http://localhost:8080/services/rest/folder/createSimple")
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path. "
			+ "Example: curl -u admin:admin -X POST -H ''Content-Type: text/plain'' -H ''Accept: application/json'' -d ''/Default/Curl/newfolder'' http://localhost:8080/services/rest/folder/createSimple")
	public WSFolder createSimple(
			@Parameter(description = "The string representing the path to be created", example = "/Default/newfolder")
			String folderPath) throws Exception {
		log.debug("createSimple()");

		String sid = validateSession();

		WSFolder root = super.getRootFolder(sid);
		return super.createPath(sid, root.getId(), folderPath);
	}

	@POST
	@Path("/createPath")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	// @ApiOperation(value = "Creates a path", notes = "Creates a path of
	// folders starting from a parent folder. It returns the metadata object
	// representing the latest created folder in the path")
	@Operation(summary = "Creates a path", description = "Creates a path of folders starting from a parent folder. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createPath(@Parameter(description = "The parent folder ID from which the new path will start")
	@FormParam("parentId")
	long parentId, @Parameter(description = "The path to create", example = "How/to/POST/JSON/data/with/Curl")
	@FormParam("path")
	String path) throws Exception {
		String sid = validateSession();
		return super.createPath(sid, parentId, path);
	}

	@GET
	@Path("/findByPath")
	@Produces({ MediaType.APPLICATION_JSON })
	// @ApiOperation(value = "Gets a folder", notes = "Finds the folder at the
	// specified path")
	@Operation(summary = "Gets a folder", description = "Finds the folder at the specified path")
	public WSFolder findByPath(@QueryParam("path")
	String path) throws Exception {
		String sid = validateSession();
		return super.findByPath(sid, path);
	}

	@GET
	@Path("/getRootFolder")
	@Produces({ MediaType.APPLICATION_JSON })
	// @ApiOperation(value = "Gets the root folder", notes = "Gets the root
	// folder in the current tenant")
	@Operation(summary = "Gets the root folder", description = "Gets the root folder in the current tenant")
	public WSFolder getRootFolder() throws Exception {
		String sid = validateSession();
		return super.getRootFolder(sid);
	}

	@POST
	@Path("/createFolder")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	// @ApiOperation(value = "Creates a subfolder")
	@Operation(summary = "Creates a subfolder")
	public long createFolder(@Parameter(description = "The ID of the parent folder")
	@FormParam("parentId")
	long parentId, @Parameter(description = "Name of the new folder")
	@FormParam("name")
	String name) throws Exception {
		String sid = validateSession();
		return super.createFolder(sid, parentId, name);
	}

	@GET
	@Path("/getFolder")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	// @ApiOperation(value = "Gets a folder", notes = "Gets the folder with the
	// specified ID")
	@Operation(summary = "Gets a folder", description = "Gets the folder with the specified ID")
	public WSFolder getFolder(@Parameter(description = "The folder ID", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.getFolder(sid, folderId);
	}

	@DELETE
	@Path("/delete")
	// @ApiOperation(nickname = "deleteFolder", value = "Deletes a folder")
	@Operation(operationId = "deleteFolder", summary = "Deletes a folder")
	public void delete(@Parameter(description = "The ID of the folder to be deleted", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		super.delete(sid, folderId);
	}

	@GET
	@Path("/listChildren")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Lists the child folders", description = "Returns the list of child folders. Example: curl -u admin:admin -H ''Accept: application/json'' http://localhost:8080/services/rest/folder/listChildren?folderId=4")
	public WSFolder[] listChildren(@Parameter(description = "The ID of the parent folder", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.listChildren(sid, folderId);
	}

	@GET
	@Path("/getPath")
	// @ApiOperation(value = "Gets a path of folders", notes = "Returns the
	// folders that make up the path to the folder in input.")
	@Operation(summary = "Gets a path of folders", description = "Returns the folders that make up the path to the folder in input")
	public WSFolder[] getPath(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.getPath(sid, folderId);
	}

	@GET
	@Path("/getPathString")
	// @ApiOperation(value = "Gets a path", notes = "Returns the path to the
	// folder in input.")
	@Operation(summary = "Gets a path", description = "Returns the path to the folder in input")
	public String getPathString(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		WSFolder[] sss = this.getPath(sid, folderId);
		String pathString = "";
		for (WSFolder wsFolder : sss) {
			pathString += "/" + wsFolder.getName();
		}
		return pathString;
	}

	@POST
	@Path("/update")
	// @ApiOperation(nickname = "updateFolder", value = "Updates a folder",
	// notes = "Updates a folder changing its metadata. The folder object in
	// input must specify the property id")
	@Operation(operationId = "updateFolder", summary = "Updates a folder", description = "Updates a folder changing its metadata. The folder object in input must specify the property id")
	public void update(
			@Parameter(description = "A value object with the metadata of the folder to update", example = "{ \"id\": 2335253, \"name\": \"Folder updated by REST ws\"}")
			WSFolder folder) throws Exception {
		log.debug("update()");
		String sid = validateSession();
		super.update(sid, folder);
	}

	@PUT
	@Path("/rename")
	// @ApiOperation(nickname = "renameFolder", value = "Renames a folder",
	// notes = "Changes the name of a given folder")
	@Operation(operationId = "renameFolder", summary = "Renames a folder", description = "Changes the name of a given folder")
	public void rename(@QueryParam("folderId")
	long folderId, @QueryParam("name")
	String name) throws Exception {
		String sid = validateSession();
		super.rename(sid, folderId, name);
	}

	@PUT
	@Path("/move")
	// @ApiOperation(nickname = "moveFolder", value = "Moves a folder", notes =
	// "Updates a folder by changing its parent. The folder is moved to the new
	// parent folder.")
	@Operation(operationId = "moveFolder", summary = "Moves a folder", description = "Updates a folder by changing its parent. The folder is moved to the new parent folder")
	public void move(@QueryParam("folderId")
	long folderId, @QueryParam("parentId")
	long parentId) throws Exception {
		String sid = validateSession();
		super.move(sid, folderId, parentId);
	}

	@Override
	@POST
	@Path("/createAlias")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	// @ApiOperation(nickname = "createAliasFolder", value = "Creates a new
	// folder alias", notes = "Creates a new alias/simlink of the source folder
	// into the target folder; returns the newly created alias")
	@Operation(operationId = "createAliasFolder", summary = "Creates a new folder alias", description = "Creates a new alias/simlink of the source folder into the target folder; returns the newly created alias")
	public WSFolder createAlias(@Parameter(description = "Folder ID in which the alias will be created (target)")
	@FormParam("parentId")
	long parentId, @Parameter(description = "Folder ID of the referenced folder (source)")
	@FormParam("foldRef")
	long foldRef) throws Exception {
		String sid = validateSession();
		return super.createAlias(sid, parentId, foldRef);
	}

	@Override
	@GET
	@Path("/getDefaultWorkspace")
	// @ApiOperation(value = "Gets the Default workspace")
	@Operation(summary = "Gets the Default workspace")
	public WSFolder getDefaultWorkspace() throws Exception {
		String sid = validateSession();
		return super.getDefaultWorkspace(sid);
	}

	@Override
	@GET
	@Path("/listWorkspaces")
	// @ApiOperation(value = "Retrieves the list of all workspaces")
	@Operation(summary = "Retrieves the list of all workspaces")
	public WSFolder[] listWorkspaces() throws Exception {
		String sid = validateSession();
		return super.listWorkspaces(sid);
	}

	@Override
	@GET
	@Path("/isReadable")
	// @ApiOperation(nickname = "isReadableFolder", value = "Tests if a folder
	// is readable")
	@Operation(operationId = "isReadableFolder", summary = "Tests if a folder is readable")
	public boolean isReadable(@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.isReadable(sid, folderId);
	}

	@Override
	@GET
	@Path("/isWritable")
	// @ApiOperation(value = "Tests if a folder is writable")
	@Operation(summary = "Tests if a folder is writable")
	public boolean isWritable(@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.isWritable(sid, folderId);
	}

	@Override
	@GET
	@Path("/isGranted")
	// @ApiOperation(value = "Tests user permission on a folder", notes = "Tests
	// if the current user has a specific permission on a folder")
	@Operation(summary = "Tests user permission on a folder", description = "Tests if the current user has a specific permission on a folder")
	public boolean isGranted(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId, @Parameter(description = "the permissions' integer representation", required = true)
	@QueryParam("permission")
	int permission) throws Exception {
		String sid = validateSession();
		return super.isGranted(sid, folderId, permission);
	}

	@Override
	@POST
	@Path("/copy")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	// @ApiOperation(value = "Copies a folder", notes = "Copies an existing
	// folder (source) into the given target; it can copy all the documents
	// contained while defining the inheritance of security policies")
	@Operation(summary = "Copies a folder", description = "Copies an existing folder (source) into the given target; it can copy all the documents contained while defining the inheritance of security policies")
	public void copy(@Parameter(description = "Folder identifier (source)", required = true)
	@FormParam("folderId")
	long folderId, @Parameter(description = "The folder ID of the target folder", required = true)
	@FormParam("targetId")
	long targetId,
			@Parameter(description = "If 1, only the folders will be copied and not the documents", required = true)
			@FormParam("foldersOnly")
			int foldersOnly,
			@Parameter(description = "<b>null</b> or </b>none</b> = no sec. policies are created, <b>inherit</b>: the new folder will point to the parent for the security policies, <b>replicate</b> = sec. policies are inherited from the new parent folder", required = false)
			@FormParam("securityOption")
			String securityOption) throws Exception {
		String sid = validateSession();
		super.copy(sid, folderId, targetId, foldersOnly, securityOption);
	}

	@Override
	@PUT
	@Path("/grantUser")
	// @ApiOperation(value = "Grants user permission to the folder")
	@Operation(summary = "Grants user permission to the folder")
	public void grantUser(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId, @Parameter(description = "User identifier (ID)", required = true)
	@QueryParam("userId")
	long userId,
			@Parameter(description = "the permission integer representation. If '0', the user will be not granted to access the folder", required = true)
			@QueryParam("permissions")
			int permissions,
			@Parameter(description = "recursion option. If true, the grant operation is applied also to the subfolders", required = true)
			@QueryParam("recursive")
			boolean recursive) throws Exception {
		String sid = validateSession();
		super.grantUser(sid, folderId, userId, permissions, recursive);
	}

	@Override
	@PUT
	@Path("/grantGroup")
	// @ApiOperation(value = "Grants group permission to the folder")
	@Operation(summary = "Grants group permission to the folde")
	public void grantGroup(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId, @Parameter(description = "Group identifier (ID)", required = true)
	@QueryParam("groupId")
	long groupId,
			@Parameter(description = "the permission integer representation. If '0', the group will be not granted to access the folder", required = true)
			@QueryParam("permissions")
			int permissions,
			@Parameter(description = "recursion option. If true, the grant operation is applied also to the subfolders", required = true)
			@QueryParam("recursive")
			boolean recursive) throws Exception {
		String sid = validateSession();
		super.grantGroup(sid, folderId, groupId, permissions, recursive);
	}

	@Override
	@GET
	@Path("/getGrantedGroups")
	// @ApiOperation(value = "Retrieves the list of granted groups for the given
	// folder")
	@Operation(summary = "Retrieves the list of granted groups for the given folder")
	public WSRight[] getGrantedGroups(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.getGrantedGroups(sid, folderId);
	}

	@Override
	@GET
	@Path("/getGrantedUsers")
	// @ApiOperation(value = "Retrieves the list of granted users for the given
	// folder")
	@Operation(summary = "Retrieves the list of granted users for the given folder")
	public WSRight[] getGrantedUsers(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws Exception {
		String sid = validateSession();
		return super.getGrantedUsers(sid, folderId);
	}

}