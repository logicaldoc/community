package com.logicaldoc.webservice.rest.endpoint;

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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.rest.DocumentMetadataService;
import com.logicaldoc.webservice.soap.endpoint.SoapDocumentMetadataService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "documentMetadata")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestDocumentMetadataService extends SoapDocumentMetadataService implements DocumentMetadataService {

	protected static Logger log = LoggerFactory.getLogger(RestDocumentMetadataService.class);

	@Override
	@PUT
	@Path("/setAttributeOptions")
	@Operation(summary = "Save attribute options", description = "Updates the options for the given attribute")
	public void setAttributeOptions(@QueryParam("setId")
	long setId, @QueryParam("attribute")
	String attribute, @QueryParam("options")
	WSAttributeOption[] options) throws WebserviceException, PersistenceException {
		String sid = validateSession();
		super.setAttributeOptions(sid, setId, attribute, options);
	}

	@Override
	@POST
	@Path("/storeAttributeSet")
	@Operation(summary = "Create/Update an attribute set", description = "Create/Update an attribute set. You can completely customize the attribute set through a value object containing the attribute set's metadata")
	public long storeAttributeSet(WSAttributeSet attributeSet) throws WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.storeAttributeSet(sid, attributeSet);
	}

	@Override
	@POST
	@Path("/storeTemplate")
	@Operation(summary = "Create/Update a template", description = "Create/Update a template. You can completely customize the template through a value object")
	public long storeTemplate(WSTemplate template)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.storeTemplate(sid, template);
	}

	@Override
	@GET
	@Path("/getAttributeSetById")
	@Operation(summary = "Gets attribute set's metadata", description = "Gets attribute set's metadata. Returns a value object containing the attribute set's metadata")
	public WSAttributeSet getAttributeSetById(@Parameter(description = "Attribute set identifier (ID)")
	@QueryParam("setId")
	long setId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.getAttributeSetById(sid, setId);
	}

	@Override
	@GET
	@Path("/getAttributeSet")
	@Operation(summary = "Gets an attribute set by name", description = "Gets an attribute set by name. Returns a value object containing the attribute set's metadata")
	public WSAttributeSet getAttributeSet(@Parameter(description = "The attribute set's name")
	@QueryParam("name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.getAttributeSet(sid, name);
	}

	@Override
	@GET
	@Path("/getTemplate")
	@Operation(summary = "Gets template's metadata", description = "Gets an existing template by its name")
	public WSTemplate getTemplate(@Parameter(description = "The template name")
	@QueryParam("name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.getTemplate(sid, name);
	}

	@Override
	@GET
	@Path("/getTemplateById")
	@Operation(summary = "Gets template's metadata", description = "Gets an existing template by its identifier")
	public WSTemplate getTemplateById(@Parameter(description = "The template identifier (ID)")
	@QueryParam("templateId")
	long templateId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.getTemplateById(sid, templateId);
	}

	@Override
	@GET
	@Path("/getAttributeOptions")
	@Operation(summary = "Retrieves the options for the given attribute", description = "Returns the list of all the attribute's options")
	public String[] getAttributeOptions(@Parameter(description = "Attribute set identifier (ID)")
	@QueryParam("setId")
	long setId, @Parameter(description = "The attribute's name")
	@QueryParam("attribute")
	String attribute) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.getAttributeOptions(sid, setId, attribute);
	}

	@Override
	@GET
	@Path("/listAttributeSets")
	@Operation(summary = "Lists the attribute sets", description = "Gets metadata of all existing attribute sets")
	public WSAttributeSet[] listAttributeSets()
			throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.listAttributeSets(sid);
	}

	@Override
	@DELETE
	@Path("/deleteAttributeSet")
	@Operation(summary = "Deletes an attribute set", description = "GDeletes an existing attribute set with the given identifier")
	public void deleteAttributeSet(@Parameter(description = "Attribute set identifier (ID)")
	@QueryParam("setId")
	long setId) throws WebserviceException, PersistenceException {
		String sid = validateSession();
		super.deleteAttributeSet(sid, setId);
	}

	@Override
	@DELETE
	@Path("/deleteTemplate")
	@Operation(summary = "Deletes a template", description = "Deletes an existing template with the given identifier")
	public void deleteTemplate(@Parameter(description = "A template ID")
	@QueryParam("templateId")
	long templateId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSession();
		super.deleteTemplate(sid, templateId);
	}

	@Override
	@GET
	@Path("/listTemplates")
	@Operation(summary = "Lists all the templates", description = "Gets metadata of all existing templates")
	public WSTemplate[] listTemplates() throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSession();
		return super.listTemplates(sid);
	}

	@Override
	@POST
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Path("/setAttributeOptionsPOST")
	@Operation(summary = "Save attribute options with a POST method", description = "Saves the options for the given attribute with a POST method. This is useful for very large lists of values")
	@RequestBody(content = @Content(mediaType = MediaType.MULTIPART_FORM_DATA, schema = @Schema(implementation = SetAttributeOptionsMultipartRequest.class)))
	public void setAttributeOptionsPOST(@Multipart(value = "setId")
	Long setId, @Multipart(value = "attribute")
	String attribute, @Multipart(value = "options", type = "application/json")
	WSAttributeOption[] options) throws WebserviceException, PersistenceException {

//		log.debug("setAttributeOptionsPOST");

		String sid = validateSession();

//		log.debug("setId: {}", setId);
//		log.debug("attribute: {}", attribute);
//		log.debug("options: {}", (Object[])options);
//		
//		if (options != null) {
//			for (WSAttributeOption wso : options) {
//				log.debug("getValue: {}", wso.getValue());
//				log.debug("getCategory: {}", wso.getCategory());
//			}
//		}

		super.setAttributeOptions(sid, setId, attribute, options);
	}

	public class SetAttributeOptionsMultipartRequest {

		@Schema(type = "integer", required = true, format = "int64", description = "Attribute set ID")
		public String setId;

		@Schema(type = "string", required = true, description = "Attribute name")
		public String attribute;

		public WSAttributeOption[] options;
	}

	@Override
	@POST
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	@Path("/addAttributeOption")
	@Operation(summary = "Adds a new attribute option with a POST method", description = "Adds the new option for the given attribute with a POST method.")
	public void addAttributeOption(@FormParam("setId")
	long setId, @FormParam("attribute")
	String attribute, @FormParam("option")
	WSAttributeOption option) throws AuthenticationException, WebserviceException, PersistenceException {

//		log.debug("addAttributeOption");

		String sid = validateSession();

//		log.debug("setId: {}", setId);
//		log.debug("attribute: {}", attribute);
//		
//		if (option != null) {
//			log.debug("value: {}", option.getValue());
//			log.debug("category: {}", option.getCategory());
//		}

		super.addAttributeOption(sid, setId, attribute, option);
	}
}
