package com.logicaldoc.webservice.rest.endpoint;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

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

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;
import com.logicaldoc.webservice.rest.TagService;
import com.logicaldoc.webservice.soap.endpoint.SoapTagService;

@Path("/")
@Api(value = "tag")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestTagService extends SoapTagService implements TagService {
	protected static Logger log = LoggerFactory.getLogger(RestTagService.class);

	@Override
	@POST
	@Path("/setDocumentTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Set the tags of a document")
	public void setDocumentTags(@ApiParam(value = "Document ID", required = true) @FormParam("docId") long docId,
			@FormParam("tag") String[] tags) throws Exception {
		String sid = validateSession();
		super.setDocumentTags(sid, docId, tags);
	}

	@Override
	@POST
	@Path("/addDocumentTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Appends new tags to a document")
	public void addDocumentTags(@ApiParam(value = "Document ID", required = true) @FormParam("docId") long docId,
			@FormParam("tag") String[] tags) throws Exception {
		String sid = validateSession();
		super.addDocumentTags(sid, docId, tags);
	}

	@Override
	@GET
	@Path("/getDocumentTags")
	@ApiOperation(value = "Gets all the tags of a document")
	public String[] getDocumentTags(@ApiParam(value = "Document ID", required = true) @QueryParam("docId") long docId)
			throws Exception {
		String sid = validateSession();
		return super.getDocumentTags(sid, docId);
	}

	@Override
	@POST
	@Path("/setFolderTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Sets the tags of a folder")
	public void setFolderTags(@ApiParam(value = "Folder ID", required = true) @FormParam("folderId") long folderId,
			@FormParam("tag") String[] tags) throws Exception {
		String sid = validateSession();
		super.setFolderTags(sid, folderId, tags);
	}

	@Override
	@POST
	@Path("/addFolderTags")
	@Consumes({ MediaType.APPLICATION_FORM_URLENCODED })
	@ApiOperation(value = "Appends new tags to a folder")
	public void addFolderTags(@ApiParam(value = "Folder ID", required = true) @FormParam("folderId") long folderId,
			@FormParam("tag") String[] tags) throws Exception {
		String sid = validateSession();
		super.addFolderTags(sid, folderId, tags);
	}

	@Override
	@GET
	@Path("/getFolderTags")
	@ApiOperation(value = "Gets all the tags of a folder")
	public String[] getFolderTags(@ApiParam(value = "Folder ID", required = true) @QueryParam("folderId") long folderId) throws Exception {
		String sid = validateSession();
		return super.getFolderTags(sid, folderId);
	}

	@Override
	@GET
	@Path("/getTags")
	@ApiOperation(value = "Gets all the tags used in the sysem")
	public String[] getTags() throws Exception {
		String sid = validateSession();
		return super.getTags(sid);
	}

	@Override
	@GET
	@Path("/getTagCloud")
	@ApiOperation(value = "Retrieves all tag clouds in the repository")
	public WSTagCloud[] getTagCloud() throws Exception {
		String sid = validateSession();
		return super.getTagCloud(sid);
	}

	@Override
	@GET
	@Path("/findDocumentsByTag")
	@ApiOperation(value = "Finds authorized documents for the current user having a specified tag.")
	public WSDocument[] findDocumentsByTag(@ApiParam(value = "The tag", required = true) @QueryParam("tag") String tag) throws Exception {
		String sid = validateSession();
		WSDocument[] docs = super.findDocumentsByTag(sid, tag);
		return docs;
	}

	@Override
	@GET
	@Path("/findFoldersByTag")
	@ApiOperation(value = "Finds authorized folders for the current user having a specified tag.")
	public WSFolder[] findFoldersByTag(@ApiParam(value = "The tag", required = true) @QueryParam("tag") String tag) throws Exception {
		String sid = validateSession();
		return super.findFoldersByTag(sid, tag);
	}

	@Override
	@GET
	@Path("/getTagsPreset")
	@ApiOperation(value = "Retrieves all the tags in the preset", notes = "Retrieves all the tags specified in the preset, empty if input mode is free")	
	public String[] getTagsPreset() throws Exception {
		String sid = validateSession();
		return super.getTagsPreset(sid);
	}
}