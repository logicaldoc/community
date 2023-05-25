package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
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
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@POST
	@Path("/setDocumentTags")
	public void setDocumentTags(@FormParam("docId") long docId, @FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Adds tags to a document
	 * 
	 * @param docId identifier of the document
	 * @param tags array of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@POST
	@Path("/addDocumentTags")
	public void addDocumentTags(@FormParam("docId") long docId, @FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Retrieves all the tags of a document
	 *
	 * @param docId identifier of the document
	 * 
	 * @return The tags of the document
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@GET
	@Path("/getDocumentTags")
	public String[] getDocumentTags(@QueryParam("docId") long docId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Sets the tags of a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param tags list of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@POST
	@Path("/setFolderTags")
	public void setFolderTags(@FormParam("folderId") long folderId, @FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Adds tags to a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param tags list of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@POST
	@Path("/addFolderTags")
	public void addFolderTags(@FormParam("folderId") long folderId, @FormParam("tag") String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Retrieves all the tags of a folder
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return The tags of the folder
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@GET
	@Path("/getFolderTags")
	public String[] getFolderTags(@QueryParam("folderId") long folderId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Retrieves all the tags in the repository
	 * 
	 * @return The tags in the repository
	 * @throws WebserviceException 
	 * @throws PersistenceException 
	 * @throws AuthenticationException 
	 * 
	 * @throws Exception error in the server application
	 */
	@GET
	@Path("/getTags")
	public String[] getTags() throws AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Retrieves all tag clouds in the repository
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTagCloud")
	public WSTagCloud[] getTagCloud() throws AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Finds authorized documents for the current user having a specified tag
	 * 
	 * @param tag Tag of the document
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session

	 */
	@GET
	@Path("/findDocumentsByTag")
	public WSDocument[] findDocumentsByTag(@QueryParam("tag") String tag) throws AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Finds authorized folders for the current user having a specified tag.
	 * 
	 * @param tag Tag of the folder
	 * 
	 * @return Collection of found folders
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/findFoldersByTag")
	public WSFolder[] findFoldersByTag(@QueryParam("tag") String tag) throws AuthenticationException, WebserviceException, PersistenceException;
	
	/**
	 * Retrieves all the tags in the preset (if the input mode is preset).
	 * 
	 * @return The tags in the preset
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTagsPreset")
	public String[] getTagsPreset() throws AuthenticationException, WebserviceException, PersistenceException;	
}