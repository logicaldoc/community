package com.logicaldoc.webservice.rest;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

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
	 * @param tags list of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@POST
	@Path("/setDocumentTags")
	public void setDocumentTags(@FormParam("docId")
	long docId, @FormParam("tag")
	List<String> tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Adds tags to a document
	 * 
	 * @param docId identifier of the document
	 * @param tags list of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws UnexistingResourceException The indicated document does not exist
	 */
	@POST
	@Path("/addDocumentTags")
	public void addDocumentTags(@FormParam("docId")
	long docId, @FormParam("tag")
	List<String> tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

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
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@GET
	@Path("/getDocumentTags")
	public List<String> getDocumentTags(@QueryParam("docId")
	long docId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException,
			UnexistingResourceException;

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
	public void setFolderTags(@FormParam("folderId")
	long folderId, @FormParam("tag")
	List<String> tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

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
	public void addFolderTags(@FormParam("folderId")
	long folderId, @FormParam("tag")
	List<String> tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

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
	public List<String> getFolderTags(@QueryParam("folderId")
	long folderId) throws PermissionException, AuthenticationException, PersistenceException, WebserviceException;

	/**
	 * Retrieves all the tags in the repository
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws WebserviceException Error in the webservice
	 * @throws PersistenceException Error in the database
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTags")
	public List<String> getTags() throws AuthenticationException, PersistenceException, WebserviceException;

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
	public List<WSTagCloud> getTagCloud() throws AuthenticationException, PersistenceException, WebserviceException;

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
	 * 
	 */
	@GET
	@Path("/findDocumentsByTag")
	public List<WSDocument> findDocumentsByTag(@QueryParam("tag")
	String tag) throws AuthenticationException, PersistenceException, WebserviceException;

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
	public List<WSFolder> findFoldersByTag(@QueryParam("tag")
	String tag) throws AuthenticationException, WebserviceException, PersistenceException;

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
	public List<String> getTagsPreset() throws AuthenticationException, WebserviceException, PersistenceException;
}