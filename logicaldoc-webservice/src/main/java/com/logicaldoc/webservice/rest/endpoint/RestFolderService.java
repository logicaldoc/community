package com.logicaldoc.webservice.rest.endpoint;

import java.util.HashMap;
import java.util.List;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSFolder;
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

	private static final Logger log = LoggerFactory.getLogger(RestFolderService.class);

	@Override
	@POST
	@Path("/create")
	@Operation(operationId = "createFolderByModel", summary = "Creates a new folder", description = "The 'folder' metadata comes in the POST request body (encoded as JSON). Note: folder object must specify at least fields name and parentId")
	public WSFolder create(
			@Parameter(description = "The folder metadata", required = true, example = "{ \"parentId\": 4, \"name\": \"Folder created by REST ws\"}", schema = @Schema(implementation = WSFolder.class))
			WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.create(sid, folder);
	}

	@Override
	@POST
	@Path("/createSimpleForm")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createSimpleForm(
			@Parameter(description = "The string representing the path to be created", example = "/Default/newfolder")
			@FormParam("folderPath")
			String folderPath)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		WSFolder root = super.getRootFolder(sid);
		return super.createPath(sid, root.getId(), folderPath);
	}

	@Override
	@POST
	@Path("/createSimpleJSON")
	@Consumes({ MediaType.APPLICATION_JSON })
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createSimpleJSON(
			@Parameter(name = "folderPath", description = "The string representing the path to be created", example = "{\"folderPath\":\"/Default/central/repo\"}")
			String jsonstr)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();

		ObjectMapper mapper = new ObjectMapper();
		TypeReference<HashMap<String, String>> typeRef = new TypeReference<HashMap<String, String>>() {
			// Nothing to do
		};
		HashMap<String, String> hm;
		try {
			hm = mapper.readValue(jsonstr, typeRef);
		} catch (JsonProcessingException e) {
			throw new WebserviceException(e.getMessage(), e);
		}

		String folderPath = hm.get("folderPath");

		WSFolder root = super.getRootFolder(sid);
		return super.createPath(sid, root.getId(), folderPath);
	}

	@Override
	@POST
	@Path("/createSimple")
	@Consumes({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
	@Operation(summary = "Creates folders from path", description = "Creates folders using an input path. All the folders in the path will be created. It returns the metadata object representing the latest created folder in the path. "
			+ "Example: curl -u admin:admin -X POST -H ''Content-Type: text/plain'' -H ''Accept: application/json'' -d ''/Default/Curl/newfolder'' http://localhost:8080/services/rest/folder/createSimple")
	public WSFolder createSimple(
			@Parameter(description = "The string representing the path to be created", example = "/Default/newfolder")
			String folderPath)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		log.debug("createSimple()");

		String sid = validateSessionREST();

		WSFolder root = super.getRootFolder(sid);
		return super.createPath(sid, root.getId(), folderPath);
	}

	@Override
	@POST
	@Path("/createPath")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Creates a path", description = "Creates a path of folders starting from a parent folder. It returns the metadata object representing the latest created folder in the path")
	public WSFolder createPath(@Parameter(description = "The parent folder ID from which the new path will start")
	@FormParam("parentId")
	long parentId, @Parameter(description = "The path to create", example = "How/to/POST/JSON/data/with/Curl")
	@FormParam("path")
	String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.createPath(sid, parentId, path);
	}

	@Override
	@GET
	@Path("/findByPath")
	@Produces({ MediaType.APPLICATION_JSON })
	@Operation(summary = "Gets a folder", description = "Finds the folder at the specified path")
	public WSFolder findByPath(@QueryParam("path")
	String path) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.findByPath(sid, path);
	}

	@Override
	@GET
	@Path("/getRootFolder")
	@Produces({ MediaType.APPLICATION_JSON })
	@Operation(summary = "Gets the root folder", description = "Gets the root folder in the current tenant")
	public WSFolder getRootFolder() throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getRootFolder(sid);
	}

	@Override
	@POST
	@Path("/createFolder")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Creates a subfolder")
	public long createFolder(@Parameter(description = "The ID of the parent folder")
	@FormParam("parentId")
	long parentId, @Parameter(description = "Name of the new folder")
	@FormParam("name")
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.createFolder(sid, parentId, name);
	}

	@Override
	@GET
	@Path("/getFolder")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Gets a folder", description = "Gets the folder with the specified ID")
	public WSFolder getFolder(@Parameter(description = "The folder ID", required = true)
	@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getFolder(sid, folderId);
	}

	@Override
	@DELETE
	@Path("/delete")
	@Operation(operationId = "deleteFolder", summary = "Deletes a folder")
	public void delete(@Parameter(description = "The ID of the folder to be deleted", required = true)
	@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.delete(sid, folderId);
	}

	@Override
	@GET
	@Path("/listChildren")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Lists the child folders", description = "Returns the list of child folders. Example: curl -u admin:admin -H ''Accept: application/json'' http://localhost:8080/services/rest/folder/listChildren?folderId=4")
	public List<WSFolder> listChildren(@Parameter(description = "The ID of the parent folder", required = true)
	@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.listChildren(sid, folderId);
	}
	
	@Override
	@GET
	@Path("/list")
	@Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
	@Operation(summary = "Lists the sub-folders", description = "Returns the list of child folders. Example: curl -u admin:admin -H ''Accept: application/json'' http://localhost:8080/services/rest/folder/list?folderId=4")
	public List<WSFolder> list(@Parameter(description = "The ID of the parent folder", required = true)
	@QueryParam("folderId")
	long folderId, @QueryParam("sort")
	String sort, @QueryParam("page")
	Integer page, @QueryParam("max")
	Integer max) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.list(sid, folderId, sort, page, max);
	}

	@Override
	@GET
	@Path("/getPath")
	@Operation(summary = "Gets a path of folders", description = "Returns the folders that make up the path to the folder in input")
	public List<WSFolder> getPath(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getPath(sid, folderId);
	}

	@Override
	@GET
	@Path("/getPathString")
	@Operation(summary = "Gets a path", description = "Returns the path to the folder in input")
	public String getPathString(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		List<WSFolder> sss = this.getPath(sid, folderId);
		StringBuilder pathString = new StringBuilder();
		for (WSFolder wsFolder : sss) {
			pathString.append("/");
			pathString.append(wsFolder.getName());
		}
		return pathString.toString();
	}

	@Override
	@POST
	@Path("/update")
	@Operation(operationId = "updateFolder", summary = "Updates a folder", description = "Updates a folder changing its metadata. The folder object in input must specify the property id")
	public void update(
			@Parameter(description = "A value object with the metadata of the folder to update", example = "{ \"id\": 2335253, \"name\": \"Folder updated by REST ws\"}")
			WSFolder folder)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		log.debug("update()");
		String sid = validateSessionREST();
		super.update(sid, folder);
	}

	@Override
	@PUT
	@Path("/rename")
	@Operation(operationId = "renameFolder", summary = "Renames a folder", description = "Changes the name of a given folder")
	public void rename(@QueryParam("folderId")
	long folderId, @QueryParam("name")
	String name) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.rename(sid, folderId, name);
	}

	@Override
	@PUT
	@Path("/move")
	@Operation(operationId = "moveFolder", summary = "Moves a folder", description = "Updates a folder by changing its parent. The folder is moved to the new parent folder")
	public void move(@QueryParam("folderId")
	long folderId, @QueryParam("parentId")
	long parentId) throws AuthenticationException, PersistenceException, WebserviceException, PermissionException {
		String sid = validateSessionREST();
		super.move(sid, folderId, parentId);
	}

	@Override
	@POST
	@Path("/createAlias")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(operationId = "createAliasFolder", summary = "Creates a new folder alias", description = "Creates a new alias/simlink of the source folder into the target folder; returns the newly created alias")
	public WSFolder createAlias(@Parameter(description = "Folder ID in which the alias will be created (target)")
	@FormParam("parentId")
	long parentId, @Parameter(description = "Folder ID of the referenced folder (source)")
	@FormParam("foldRef")
	long foldRef) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.createAlias(sid, parentId, foldRef);
	}

	@Override
	@GET
	@Path("/getDefaultWorkspace")
	@Operation(summary = "Gets the Default workspace")
	public WSFolder getDefaultWorkspace()
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getDefaultWorkspace(sid);
	}

	@Override
	@GET
	@Path("/listWorkspaces")
	@Operation(summary = "Retrieves the list of all workspaces")
	public List<WSFolder> listWorkspaces() throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.listWorkspaces(sid);
	}

	@Override
	@GET
	@Path("/isReadable")
	@Operation(operationId = "isReadableFolder", summary = "Tests if a folder is readable")
	public boolean isReadable(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isReadable(sid, folderId);
	}

	@Override
	@GET
	@Path("/isWritable")
	@Operation(summary = "Tests if a folder is writable")
	public boolean isWritable(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isWritable(sid, folderId);
	}

	@Override
	@GET
	@Path("/isGranted")
	@Operation(summary = "Tests user permission on a folder", description = "Tests if the current user has a specific permission on a folder")
	public boolean isGranted(@Parameter(description = "Folder identifier (ID)", required = true)
	@QueryParam("folderId")
	long folderId, @Parameter(description = "the permissions' integer representation", required = true)
	@QueryParam("permission")
	String permission) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.isGranted(sid, folderId, permission);
	}

	@Override
	@POST
	@Path("/copy")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
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
			String securityOption)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		String sid = validateSessionREST();
		super.copy(sid, folderId, targetId, foldersOnly, securityOption);
	}

	@Override
	@PUT
	@Path("/setAccessControlList")
	@Operation(operationId = "setAccessControlList_Folder", summary = "Assigns the complete Access Control List")
	public void setAccessControlList(@QueryParam("folderId")
	long folderId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		String sid = validateSessionREST();
		super.setAccessControlList(sid, folderId, acl);
	}

	@Override
	@GET
	@Path("/getAccessControlList")
	@Operation(operationId = "getAccessControlList_Folder", summary = "Retrieves the access control list")
	public List<WSAccessControlEntry> getAccessControlList(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		String sid = validateSessionREST();
		return super.getAccessControlList(sid, folderId);
	}
}