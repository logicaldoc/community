package com.logicaldoc.webservice.soap;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebResult;
import jakarta.jws.WebService;

@WSDoc(description = "handling of templates and attribute sets")
@WebService(name = "DocumentMetadata", serviceName = "DocumentMetadata", targetNamespace = "http://ws.logicaldoc.com")
public interface DocumentMetadataService {

	/**
	 * Gets metadata of all existing templates.
	 * 
	 * @param sid Session identifier
	 * 
	 * @return The list of all templates
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "template")
	@WebMethod(action = "listTemplates")
	@WSDoc(description = "lists all the templates")
	public List<WSTemplate> listTemplates(@WebParam(name = "sid")
	String sid) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets template's metadata
	 * 
	 * @param sid Session identifier
	 * @param name The template's name
	 * 
	 * @return A value object containing the template's metadata.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid sessionon
	 */
	@WebResult(name = "template")
	@WebMethod(action = "getTemplate")
	@WSDoc(description = "gets an existing template by it's name")
	public WSTemplate getTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets template's metadata
	 * 
	 * @param sid Session identifier
	 * @param templateId The template's id
	 * 
	 * @return A value object containing the template's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "template")
	@WebMethod(action = "getTemplateById")
	@WSDoc(description = "gets an existing template by it's identifier")
	public WSTemplate getTemplateById(@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Create/Update a template. You can completely customize the template
	 * through a value object.
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param template value object containing the template's metadata
	 * 
	 * @return The ID of the new template
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "templateId")
	@WebMethod(action = "storeTemplate")
	@WSDoc(description = "creates/updates a template; you can completely customize the template through a value object; returns the identifier of the created/updated template")
	public long storeTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "template")
	WSTemplate template) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Deletes an existing template with the given identifier
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param templateId The template's id
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "deleteTemplate")
	@WSDoc(description = "deletes an existing template")
	public void deleteTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Gets metadata of all existing attribute sets.
	 * 
	 * @param sid Session identifier
	 * @return The list of all attribute sets
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "attributeSet")
	@WebMethod(action = "listAttributeSets")
	@WSDoc(description = "lists all the attribute sets")
	public List<WSAttributeSet> listAttributeSets(@WebParam(name = "sid")
	String sid) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param sid Session identifier
	 * @param name The attribute set's name
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "attributeSet")
	@WebMethod(action = "getAttributeSet")
	@WSDoc(description = "gets an attribute set by it's name")
	public WSAttributeSet getAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebResult(name = "attributeSet")
	@WebMethod(action = "getAttributeSetById")
	@WSDoc(description = "gets an attribute set by it's identifier")
	public WSAttributeSet getAttributeSetById(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Create/Update an attribute set. You can completely customize the
	 * attribute set through a value object containing the attribute set's
	 * metadata.
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param attributeSet set's value object containing the attribute set's
	 *        metadata
	 * 
	 * @return The ID of the new attribute set
	 *
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebResult(name = "setId")
	@WebMethod(action = "storeAttributeSet")
	@WSDoc(description = "creates/updates an attribute set; you can completely customize the set through a value object; returns the identifier of the created/updated set")
	public long storeAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "attributeSet")
	WSAttributeSet attributeSet) throws WebserviceException, PersistenceException;

	/**
	 * Deletes an existing attribute set with the given identifier.
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param setId The attribute set's id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebMethod(action = "deleteAttributeSet")
	@WSDoc(description = "deletes an existing attribute set")
	public void deleteAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId) throws WebserviceException, PersistenceException;

	/**
	 * Saves the options for the given attribute
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param options The attribute's options
	 *
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@WebMethod(action = "setAttributeOptions")
	@WSDoc(description = "saves the options for the given attribute")
	public void setAttributeOptions(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute, @WebParam(name = "options")
	List<WSAttributeOption> options) throws WebserviceException, PersistenceException;

	/**
	 * Retrieves the options for the given attribute
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * 
	 * @return the list of all the attribute's options
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getAttributeOptions")
	@WSDoc(description = "retrieves the options for the given attribute")
	public List<String> getAttributeOptions(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the options for the given attribute inside a given category
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param category The options category (not mandatory)
	 * 
	 * @return the list of all the attribute's options
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getAttributeOptionsByCategory")
	@WSDoc(description = "retrieves the options for the given attribute inside a given category")
	public List<WSAttributeOption> getAttributeOptionsByCategory(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute, @WebParam(name = "category")
	String category) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a template is readable.
	 * 
	 * @param sid Session identifier
	 * @param templateId The template id
	 * 
	 * @return True if the identifier denotes a readable template, otherwise
	 *         false.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isReadable")
	@WSDoc(description = "tests if a template is readable")
	public boolean isReadable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Tests if a template is writable
	 * 
	 * @param sid Session identifier
	 * @param templateId The template id
	 * @return True if the identifier denotes a writable template, otherwise
	 *         false
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "isWritable")
	@WSDoc(description = "tests if a template is writable")
	public boolean isWritable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Sets the Access Control List
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @param acl the complete Access Control List
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod(action = "setAccessControlList")
	@WSDoc(description = "sets the Access Control List")
	public void setAccessControlList(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId, @WSDoc(description = "the complete Access Control List")
	@WebParam(name = "acl")
	List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException;

	/**
	 * Retrieves the access control list
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * 
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getAccessControlList")
	@WSDoc(description = "retrieves the access control list")
	public List<WSAccessControlEntry> getAccessControlList(
			@WSDoc(description = "identifier of the session", required = true)
			@WebParam(name = "sid")
			String sid, @WebParam(name = "templateId")
			long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Add a new option for the given attribute
	 * 
	 * @param sid Session identifier
	 * @param setId Attribute set ID
	 * @param attribute Attribute name
	 * @param option Attribute option
	 *
	 * @throws AuthenticationException Invalid session
	 * @throws WebserviceException Error in the webservice
	 * @throws PersistenceException Error in the database
	 */
	@WebMethod(action = "addAttributeOption")
	@WSDoc(description = "Adds a new option for the given attribute")
	public void addAttributeOption(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute, @WebParam(name = "option")
	WSAttributeOption option) throws AuthenticationException, WebserviceException, PersistenceException;
}
