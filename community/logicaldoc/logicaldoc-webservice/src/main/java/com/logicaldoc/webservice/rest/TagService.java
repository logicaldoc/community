package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;

/**
 * Tag Web Service definition interface for REST.
 * 
 * Marco Meschieri - LogicalDOC
 * 
 * @since 7.6.3
 */
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface TagService {

	/**
	 * Sets the tags of a document
	 * 
	 * @param docId identifier of the document
	 * @param tags array of tags
	 * 
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/setDocumentTags")
	public void setDocumentTags(@FormParam("docId") long docId, @FormParam("tag") String[] tags) throws Exception;

	/**
	 * Adds tags to a document
	 * 
	 * @param docId identifier of the document
	 * @param tags array of tags
	 * 
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/addDocumentTags")
	public void addDocumentTags(@FormParam("docId") long docId, @FormParam("tag") String[] tags) throws Exception;

	/**
	 * Retrieves all the tags of a document
	 *
	 * @param docId identifier of the document
	 * 
	 * @return The tags of the document
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getDocumentTags")
	public String[] getDocumentTags(@QueryParam("docId") long docId) throws Exception;

	/**
	 * Sets the tags of a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param tags list of tags
	 * 
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/setFolderTags")
	public void setFolderTags(@FormParam("folderId") long folderId, @FormParam("tag") String[] tags) throws Exception;

	/**
	 * Adds tags to a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param tags list of tags
	 * 
	 * @throws Exception error in the server application
	 */
	@POST
	@Path("/addFolderTags")
	public void addFolderTags(@FormParam("folderId") long folderId, @FormParam("tag") String[] tags) throws Exception;

	/**
	 * Retrieves all the tags of a folder
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return The tags of the folder
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getFolderTags")
	public String[] getFolderTags(@QueryParam("folderId") long folderId) throws Exception;

	/**
	 * Retrieves all the tags in the repository
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getTags")
	public String[] getTags() throws Exception;

	/**
	 * Retrieves all tag clouds in the repository
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getTagCloud")
	public WSTagCloud[] getTagCloud() throws Exception;

	/**
	 * Finds authorized documents for the current user having a specified tag
	 * 
	 * @param tag Tag of the document
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/findDocumentsByTag")
	public WSDocument[] findDocumentsByTag(@QueryParam("tag") String tag) throws Exception;

	/**
	 * Finds authorized folders for the current user having a specified tag.
	 * 
	 * @param tag Tag of the folder
	 * 
	 * @return Collection of found folders
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/findFoldersByTag")
	public WSFolder[] findFoldersByTag(@QueryParam("tag") String tag) throws Exception;
	
	/**
	 * Retrieves all the tags in the preset (if the input mode is preset).
	 * 
	 * @return The tags in the preset
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getTagsPreset")
	public String[] getTagsPreset() throws Exception;	
}