package com.logicaldoc.webservice.rest.endpoint;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;
import com.logicaldoc.webservice.rest.TagService;
import com.logicaldoc.webservice.soap.endpoint.SoapTagService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "tag")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestTagService extends SoapTagService implements TagService {
	protected static Logger log = LoggerFactory.getLogger(RestTagService.class);

	@Override
	@POST
	@Path("/setDocumentTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Set the tags of a document")
	public void setDocumentTags(@Parameter(description = "Document ID", required = true) @FormParam("docId") long docId,
			@FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException{
		String sid = validateSession();
		super.setDocumentTags(sid, docId, tags);
	}

	@Override
	@POST
	@Path("/addDocumentTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Appends new tags to a document")
	public void addDocumentTags(@Parameter(description = "Document ID", required = true) @FormParam("docId") long docId,
			@FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException{
		String sid = validateSession();
		super.addDocumentTags(sid, docId, tags);
	}

	@Override
	@GET
	@Path("/getDocumentTags")
	@Operation(summary = "Gets all the tags of a document")
	public String[] getDocumentTags(@Parameter(description = "Document ID", required = true) @QueryParam("docId") long docId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException
			 {
		String sid = validateSession();
		return super.getDocumentTags(sid, docId);
	}

	@Override
	@POST
	@Path("/setFolderTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Sets the tags of a folder")
	public void setFolderTags(
			@Parameter(description = "Folder ID", required = true) @FormParam("folderId") long folderId,
			@FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException{
		String sid = validateSession();
		super.setFolderTags(sid, folderId, tags);
	}

	@Override
	@POST
	@Path("/addFolderTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@Operation(summary = "Appends new tags to a folder")
	public void addFolderTags(
			@Parameter(description = "Folder ID", required = true) @FormParam("folderId") long folderId,
			@FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException{
		String sid = validateSession();
		super.addFolderTags(sid, folderId, tags);
	}

	@Override
	@GET
	@Path("/getFolderTags")
	@Operation(summary = "Gets all the tags of a folder")
	public String[] getFolderTags(@Parameter(description = "Folder ID", required = true) @QueryParam("folderId") long folderId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException{
		String sid = validateSession();
		return super.getFolderTags(sid, folderId);
	}

	@Override
	@GET
	@Path("/getTags")
	@Operation(summary = "Gets all the tags used in the sysem")
	public String[] getTags() throws AuthenticationException, PersistenceException, WebserviceException{
		String sid = validateSession();
		return super.getTags(sid);
	}

	@Override
	@GET
	@Path("/getTagCloud")
	@Operation(summary = "Retrieves all tag clouds in the repository")
	public WSTagCloud[] getTagCloud() throws AuthenticationException, PersistenceException, WebserviceException{
		String sid = validateSession();
		return super.getTagCloud(sid);
	}

	@Override
	@GET
	@Path("/findDocumentsByTag")
	@Operation(summary = "Finds authorized documents for the current user having a specified tag")
	public WSDocument[] findDocumentsByTag(@Parameter(description = "The tag", required = true) @QueryParam("tag") String tag) throws AuthenticationException, PersistenceException, WebserviceException{
		String sid = validateSession();
		return super.findDocumentsByTag(sid, tag);
	}

	@Override
	@GET
	@Path("/findFoldersByTag")
	@Operation(summary = "Finds authorized folders for the current user having a specified tag")
	public WSFolder[] findFoldersByTag(@Parameter(description = "The tag", required = true) @QueryParam("tag") String tag) throws AuthenticationException, WebserviceException, PersistenceException{
		String sid = validateSession();
		return super.findFoldersByTag(sid, tag);
	}

	@Override
	@GET
	@Path("/getTagsPreset")
	@Operation(
			summary = "Retrieves all the tags in the preset", 
			description = "Retrieves all the tags specified in the preset, empty if input mode is free")
	public String[] getTagsPreset() throws AuthenticationException, WebserviceException, PersistenceException{
		String sid = validateSession();
		return super.getTagsPreset(sid);
	}
}