package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;

/**
 * Tag Web Service definition interface
 * 
 * Marco Meschieri - LogicalDOC
 * 
 * @since 7.6.3
 */
@WSDoc(description = "Tags management and search")
@WebService(name = "Tag", serviceName = "Tag", targetNamespace = "http://ws.logicaldoc.com")
public interface TagService {

	/**
	 * Sets the tags of a document
	 * 
	 * @param sid identifier of the session
	 * @param docId identifier of the document
	 * @param tags array of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@WebMethod(action = "setDocumentTags")
	@WSDoc(description = "sets the tags of a document")
	public void setDocumentTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the document", required = true)
	@WebParam(name = "docId")
	long docId, @WebParam(name = "tag")
	String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Adds tags to a document
	 * 
	 * @param sid Session Identifier
	 * @param docId identifier of the document
	 * @param tags array of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 * @throws UnexistingResourceException The specified document does not exist
	 */
	@WebMethod(action = "addDocumentTags")
	@WSDoc(description = "adds tags to a document")
	public void addDocumentTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the document", required = true)
	@WebParam(name = "docId")
	long docId, @WebParam(name = "tag")
	String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException;

	/**
	 * Retrieves all the tags of a document.
	 * 
	 * @param sid identifier of the session
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
	@WebMethod(action = "getDocumentTags")
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags of a document")
	public String[] getDocumentTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WSDoc(description = "identifier of the document", required = true)
	@WebParam(name = "docId")
	long docId) throws PermissionException, PersistenceException, AuthenticationException, WebserviceException,
			UnexistingResourceException;

	/**
	 * Sets the tags of a folder
	 * 
	 * @param sid Session Identifier
	 * @param folderId identifier of the folder
	 * @param tags array of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "setFolderTags")
	@WSDoc(description = "sets the tags of a folder")
	public void setFolderTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WebParam(name = "tag")
	String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Adds tags to a folder
	 * 
	 * @param sid Session Identifier
	 * @param folderId identifier of the folder
	 * @param tags array of tags
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "addFolderTags")
	@WSDoc(description = "adds tags to a folder")
	public void addFolderTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId, @WebParam(name = "tag")
	String[] tags) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Retrieves all the tags of a folder
	 * 
	 * @param sid Session Identifier
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
	@WebMethod(action = "getFolderTags")
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags of a folder")
	public String[] getFolderTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws PermissionException, PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Retrieves all the tags in the repository
	 * 
	 * @param sid Session Identifier
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getTags")
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags in the repository")
	public String[] getTags(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Retrieves all the tags in the preset(if the input mode is preset).
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return The tags in the preset
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getTagsPreset")
	@WebResult(name = "tag")
	@WSDoc(description = "retrieves all the tags specified in the preset, empty if input mode is free")
	public String[] getTagsPreset(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves all tag clouds in the repository.
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return The tags in the repository
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getTagCloud")
	@WebResult(name = "tagCloud")
	@WSDoc(description = "retrieves all tag clouds in the repository")
	public WSTagCloud[] getTagCloud(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid) throws PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Finds authorized documents for the current user having a specified tag
	 * 
	 * @param sid identifier of the session
	 * @param tag tag of the document
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "findDocumentsByTag")
	@WebResult(name = "document")
	@WSDoc(description = "finds authorized documents for the current user having a specified tag")
	public WSDocument[] findDocumentsByTag(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "tag")
	String tag) throws PersistenceException, AuthenticationException, WebserviceException;

	/**
	 * Finds authorized folders for the current user having a specified tag
	 * 
	 * @param sid identifier of the session
	 * @param tag tag of the folder
	 * 
	 * @return Collection of found folders
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "findFoldersByTag")
	@WebResult(name = "folder")
	@WSDoc(description = "finds authorized folders for the current user having a specified tag")
	public WSFolder[] findFoldersByTag(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "tag")
	String tag) throws AuthenticationException, WebserviceException, PersistenceException;
}
