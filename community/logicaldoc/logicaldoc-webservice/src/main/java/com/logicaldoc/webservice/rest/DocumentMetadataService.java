package com.logicaldoc.webservice.rest;

import javax.jws.WebParam;
import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;

/**
 * Document Metadata Service definition interface for REST.
 * 
 * Alessandro Gasparini - LogicalDOC
 * 
 * @since 8.4.2
 */
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface DocumentMetadataService {
	

	/**
	 * Saves the options for the given attribute
	 * 

	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param values The attribute's options
	 * 
	 * @throws Exception error in the server application
	 */
	public void setAttributeOptions(long setId, String attribute, String[] values) throws Exception;
	
	/**
	 * Retrieves the options for the given attribute
	 * 
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * 
	 * @return the list of all the attribute's options
	 * 
	 * @throws Exception error in the server application
	 */
	public String[] getAttributeOptions(long setId, String attribute) throws Exception;	

	/**
	 * Create/Update an attribute set. You can completely customize the
	 * attribute set through a value object containing the attribute set's
	 * metadata.
	 * 
	 * @param attributeSet set's value object containing the attribute set's metadata
	 * 
	 * @return The ID of the new attribute set
	 * 
	 * @throws Exception error in the server application
	 */
	public long storeAttributeSet(WSAttributeSet attributeSet) throws Exception;


	/**
	 * Create/Update a template. You can completely customize the template
	 * through a value object.
	 * 
	 * @param template value object containing the template's metadata
	 * 
	 * @return The ID of the new template
	 * 
	 * @throws Exception error in the server application
	 */
	public long storeTemplate(WSTemplate template) throws Exception;
	
	
	/**
	 * Gets attribute set's metadata
	 * 
	 * @param setId The attribute set's id
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	public WSAttributeSet getAttributeSetById(long setId) throws Exception;	
	
	/**
	 * Gets attribute set's metadata
	 * 
	 * @param name The attribute set's name
	 * 
	 * @return A value object containing the attribute set's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	public WSAttributeSet getAttributeSet(String name) throws Exception;
	

	/**
	 * Gets template's metadata
	 * 
	 * @param name The template's name
	 * 
	 * @return A value object containing the template's metadata.
	 * 
	 * @throws Exception error in the server application
	 */
	public WSTemplate getTemplate(String name) throws Exception;

	/**
	 * Gets template's metadata
	 * 
	 * @param templateId The template's id
	 * 
	 * @return A value object containing the template's metadata
	 * 
	 * @throws Exception error in the server application
	 */
	public WSTemplate getTemplateById(long templateId) throws Exception;
	
	/**
	 * Gets metadata of all existing templates.
	 * 
	 * @return The list of all templates
	 * 
	 * @throws Exception error in the server application
	 */
	public WSTemplate[] listTemplates() throws Exception;

	
	/**
	 * Gets metadata of all existing attribute sets.
	 * 
	 * @return The list of all attribute sets
	 * 
	 * @throws Exception error in the server application
	 */
	public WSAttributeSet[] listAttributeSets() throws Exception;


	/**
	 * Deletes an existing attribute set with the given identifier.
	 * 
	 * @param setId The attribute set's id
	 * 
	 * @throws Exception error in the server application
	 */
	public void deleteAttributeSet(@WebParam(name = "setId") long setId) throws Exception;

	
	/**
	 * Deletes an existing template with the given identifier
	 * 
	 * @param templateId The template's id
	 * 
	 * @throws Exception error in the server application
	 */
	public void deleteTemplate(long templateId) throws Exception;	
	
}