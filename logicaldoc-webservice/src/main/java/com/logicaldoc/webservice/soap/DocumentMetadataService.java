package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSRight;
import com.logicaldoc.webservice.model.WSTemplate;

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
	public WSTemplate[] listTemplates(@WebParam(name = "sid")
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
	public WSAttributeSet[] listAttributeSets(@WebParam(name = "sid")
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
	WSAttributeOption[] options) throws WebserviceException, PersistenceException;

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
	public String[] getAttributeOptions(@WebParam(name = "sid")
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
	public WSAttributeOption[] getAttributeOptionsByCategory(@WebParam(name = "sid")
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
	@WebMethod(action = "isTemplateReadable")
	@WSDoc(description = "tests if a template is readable")
	public boolean isTemplateReadable(@WSDoc(description = "identifier of the session", required = true)
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
	@WebMethod(action = "isTemplateWritable")
	@WSDoc(description = "tests if a template is writable")
	public boolean isTemplateWritable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Grants user permission to the template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @param userId User Id
	 * @param permissions the permission integer representation. If '0', the
	 *        user will be not granted to access the template.
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "grantUserToTemplate")
	@WSDoc(description = "grants user permission to the template")
	public void grantUserToTemplate(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId, @WebParam(name = "userId")
	long userId,
			@WSDoc(description = "the permission integer representation; if '0', the user will be not granted to access the template")
			@WebParam(name = "permissions")
			int permissions)
			throws PersistenceException, AuthenticationException, WebserviceException, PermissionException;

	/**
	 * Grants group permission to the template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @param groupId Group Id
	 * @param permissions the permission integer representation. If '0', the
	 *        group will be not granted to access the template.
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "grantGroupToTemplate")
	@WSDoc(description = "grants group permission to the template")
	public void grantGroupToTemplate(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId, @WebParam(name = "groupId")
	long groupId,
			@WSDoc(description = "the permission integer representation; if '0', the group will be not granted to access the template")
			@WebParam(name = "permissions")
			int permissions)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Retrieves the list of granted users for the given template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * 
	 * @return 'error' if error occurred, the right objects collection.
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getGrantedUsers")
	@WSDoc(description = "retrieves the list of granted users for the given folder")
	public WSRight[] getGrantedUsers(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	/**
	 * Retrieves the list of granted groups for the given template
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getGrantedGroups")
	@WSDoc(description = "retrieves the list of granted groups for the given folder")
	public WSRight[] getGrantedGroups(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException;

	
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
	public void addAttributeOption(
			@WebParam(name = "sid") String sid, 
			@WebParam(name = "setId") long setId, 
			@WebParam(name = "attribute") String attribute, 
			@WebParam(name = "option") WSAttributeOption option) throws AuthenticationException, WebserviceException, PersistenceException;
}
