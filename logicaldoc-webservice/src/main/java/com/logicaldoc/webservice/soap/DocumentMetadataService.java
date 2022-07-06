package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.webservice.doc.WSDoc;
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
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "template")
	@WebMethod
	@WSDoc(description = "lists all the templates")
	public WSTemplate[] listTemplates(@WebParam(name = "sid")
	String sid) throws Exception;

	/**
	 * Gets template's metadata
	 * 
	 * @param sid Session identifier
	 * @param name The template's name
	 * 
	 * @return A value object containing the template's metadata.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "template")
	@WebMethod
	@WSDoc(description = "gets an existing template by it's name")
	public WSTemplate getTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "name")
	String name) throws Exception;

	/**
	 * Gets template's metadata
	 * 
	 * @param sid Session identifier
	 * @param templateId The template's id
	 * 
	 * @return A value object containing the template's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "template")
	@WebMethod
	@WSDoc(description = "gets an existing template by it's identifier")
	public WSTemplate getTemplateById(@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws Exception;

	/**
	 * Create/Update a template. You can completely customize the template
	 * through a value object.
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param template value object containing the template's metadata
	 * 
	 * @return The ID of the new template
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "templateId")
	@WebMethod
	@WSDoc(description = "creates/updates a template; you can completely customize the template through a value object; returns the identifier of the created/updated template")
	public long storeTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "template")
	WSTemplate template) throws Exception;

	/**
	 * Deletes an existing template with the given identifier
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param templateId The template's id
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "deletes an existing template")
	public void deleteTemplate(@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws Exception;

	/**
	 * Gets metadata of all existing attribute sets.
	 * 
	 * @param sid Session identifier
	 * @return The list of all attribute sets
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "attributeSet")
	@WebMethod
	@WSDoc(description = "lists all the attribute sets")
	public WSAttributeSet[] listAttributeSets(@WebParam(name = "sid")
	String sid) throws Exception;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param sid Session identifier
	 * @param name The attribute set's name
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "attributeSet")
	@WebMethod
	@WSDoc(description = "gets an attribute set by it's name")
	public WSAttributeSet getAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "name")
	String name) throws Exception;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "attributeSet")
	@WebMethod
	@WSDoc(description = "gets an attribute set by it's identifier")
	public WSAttributeSet getAttributeSetById(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId) throws Exception;

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
	 * @throws Exception error in the server application
	 */
	@WebResult(name = "setId")
	@WebMethod
	@WSDoc(description = "creates/updates an attribute set; you can completely customize the set through a value object; returns the identifier of the created/updated set")
	public long storeAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "attributeSet")
	WSAttributeSet attributeSet) throws Exception;

	/**
	 * Deletes an existing attribute set with the given identifier.
	 * 
	 * @param sid Session identifier (need to be administrator)
	 * @param setId The attribute set's id
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "deletes an existing attribute set")
	public void deleteAttributeSet(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId) throws Exception;

	/**
	 * Saves the options for the given attribute
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param values The attribute's options
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "saves the options for the given attribute")
	public void setAttributeOptions(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute, @WebParam(name = "values")
	String[] values) throws Exception;

	/**
	 * Retrieves the options for the given attribute
	 * 
	 * @param sid Session identifier
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * 
	 * @return the list of all the attribute's options
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "retrieves the options for the given attribute")
	public String[] getAttributeOptions(@WebParam(name = "sid")
	String sid, @WebParam(name = "setId")
	long setId, @WebParam(name = "attribute")
	String attribute) throws Exception;

	/**
	 * Tests if a template is readable.
	 * 
	 * @param sid Session identifier
	 * @param templateId The template id
	 * 
	 * @return True if the identifier denotes a readable template, otherwise
	 *         false.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "tests if a template is readable")
	public boolean isTemplateReadable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "templateId")
	long templateId) throws Exception;

	/**
	 * Tests if a template is writable
	 * 
	 * @param sid Session identifier
	 * @param templateId The template id
	 * @return True if the identifier denotes a writable template, otherwise
	 *         false
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "tests if a template is writable")
	public boolean isTemplateWritable(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")	String sid, @WebParam(name = "templateId")	long templateId) throws Exception;

	/**
	 * Grants user permission to the template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @param userId User Id
	 * @param permissions the permission integer representation. If '0', the
	 *        user will be not granted to access the template.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "grants user permission to the template")
	public void grantUserToTemplate(
			@WSDoc(description = "identifier of the session", required = true) @WebParam(name = "sid") String sid,
			@WebParam(name = "templateId") long templateId, @WebParam(name = "userId") long userId,
			@WSDoc(description = "the permission integer representation; if '0', the user will be not granted to access the template") @WebParam(name = "permissions") int permissions)
			throws Exception;
	
	/**
	 * Grants group permission to the template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * @param groupId Group Id
	 * @param permissions the permission integer representation. If '0', the
	 *        group will be not granted to access the template.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "grants group permission to the template")
	public void grantGroupToTemplate(
			@WSDoc(description = "identifier of the session", required = true) @WebParam(name = "sid") String sid,
			@WebParam(name = "templateId") long templateId, @WebParam(name = "groupId") long groupId,
			@WSDoc(description = "the permission integer representation; if '0', the group will be not granted to access the template") @WebParam(name = "permissions") int permissions)
			throws Exception;
	
	/**
	 * Retrieves the list of granted users for the given template.
	 * 
	 * @param sid Session identifier
	 * @param templateId Template id
	 * 
	 * @return 'error' if error occurred, the right objects collection.
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "retrieves the list of granted users for the given folder")
	public WSRight[] getGrantedUsers(
			@WSDoc(description = "identifier of the session", required = true) @WebParam(name = "sid") String sid,
			@WebParam(name = "templateId") long templateId) throws Exception;

	/**
	 * Retrieves the list of granted groups for the given template
	 * 
	 * @param sid Session identifier
	 *@param templateId Template id
	 * @return 'error' if error occurred, the right objects collection
	 * 
	 * @throws Exception error in the server application
	 */
	@WebMethod
	@WSDoc(description = "retrieves the list of granted groups for the given folder")
	public WSRight[] getGrantedGroups(
			@WSDoc(description = "identifier of the session", required = true) @WebParam(name = "sid") String sid,
			@WebParam(name = "templateId") long templateId) throws Exception;
}
