package com.logicaldoc.webservice.rest;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;

import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Document Metadata Service definition interface for REST.
 * 
 * Alessandro Gasparini - LogicalDOC
 * 
 * @since 8.4.2
 */
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
@Path("/")
@Tag(name = "documentMetadata")
public interface DocumentMetadataService {

	/**
	 * Add a new option for the given attribute
	 * 
	 * @param setId Attribute set ID
	 * @param attribute Attribute name
	 * @param option Attribute option
	 * 
	 * @throws AuthenticationException Invalid session
	 * @throws WebserviceException Error in the webservice
	 * @throws PersistenceException Error in the database
	 */
	@POST
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Path("/addAttributeOption")
	public void addAttributeOption(@FormParam("setId")
	long setId, @FormParam("attribute")
	String attribute, @FormParam("option")
	WSAttributeOption option) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Saves the options for the given attribute
	 * 
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param options The attribute's options
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@PUT
	@Path("/setAttributeOptions")
	public void setAttributeOptions(@QueryParam("setId")
	long setId, @QueryParam("attribute")
	String attribute, @QueryParam("options")
	List<WSAttributeOption> options) throws WebserviceException, PersistenceException;

	/**
	 * Saves the options for the given attribute with a POST method. This is
	 * useful for very large lists of values
	 * 
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * @param options The attribute's options
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@POST
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Path("/setAttributeOptionsPOST")
	public void setAttributeOptionsPOST(@Multipart(value = "setId")
	Long setId, @Multipart(value = "attribute")
	String attribute, @Multipart(value = "options", type = "application/json")
	List<WSAttributeOption> options) throws WebserviceException, PersistenceException;

	/**
	 * Create/Update an attribute set. You can completely customize the
	 * attribute set through a value object containing the attribute set's
	 * metadata.
	 * 
	 * @param attributeSet set's value object containing the attribute set's
	 *        metadata
	 * 
	 * @return The ID of the new attribute set
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@POST
	@Path("/storeAttributeSet")
	public long storeAttributeSet(WSAttributeSet attributeSet) throws WebserviceException, PersistenceException;

	/**
	 * Create/Update a template. You can completely customize the template
	 * through a value object.
	 * 
	 * @param template value object containing the template's metadata
	 * 
	 * @return The ID of the new template
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@POST
	@Path("/storeTemplate")
	public long storeTemplate(WSTemplate template)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param setId The attribute set's id
	 * 
	 * @return the attribute set
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getAttributeSetById")
	public WSAttributeSet getAttributeSetById(@QueryParam("setId")
	long setId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets attribute set's metadata
	 * 
	 * @param name The attribute set's name
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getAttributeSet")
	public WSAttributeSet getAttributeSet(@QueryParam("name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets template's metadata
	 * 
	 * @param name The template's name
	 * 
	 * @return A value object containing the template's metadata.
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTemplate")
	public WSTemplate getTemplate(@QueryParam("name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets template's metadata
	 * 
	 * @param templateId The template's id
	 * 
	 * @return A value object containing the template's metadata
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getTemplateById")
	public WSTemplate getTemplateById(@QueryParam("templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the options for the given attribute
	 * 
	 * @param setId The attribute set's id
	 * @param attribute The attribute's name
	 * 
	 * @return the list of all the attribute's options
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/getAttributeOptions")
	public List<String> getAttributeOptions(@QueryParam("setId")
	long setId, @QueryParam("attribute")
	String attribute) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Retrieves the options for the given attribute inside a given category
	 * 
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
	@GET
	@Path("/getAttributeOptionsByCategory")
	public List<WSAttributeOption> getAttributeOptionsByCategory(@QueryParam("setId")
	long setId, @QueryParam("attribute")
	String attribute, @QueryParam("category")
	String category) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Gets metadata of all existing attribute sets.
	 * 
	 * @return The list of all attribute sets
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/listAttributeSets")
	public List<WSAttributeSet> listAttributeSets()
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Deletes an existing attribute set with the given identifier.
	 * 
	 * @param setId The attribute set's id
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 */
	@DELETE
	@Path("/deleteAttributeSet")
	public void deleteAttributeSet(@QueryParam("setId")
	long setId) throws WebserviceException, PersistenceException;

	/**
	 * Deletes an existing template with the given identifier
	 * 
	 * @param templateId The template's id
	 * 
	 * @throws PermissionException The permission has not been granted
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@DELETE
	@Path("/deleteTemplate")
	public void deleteTemplate(@QueryParam("templateId")
	long templateId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Gets metadata of all existing templates.
	 * 
	 * @return The list of all templates
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/listTemplates")
	public List<WSTemplate> listTemplates() throws AuthenticationException, WebserviceException, PersistenceException;
}