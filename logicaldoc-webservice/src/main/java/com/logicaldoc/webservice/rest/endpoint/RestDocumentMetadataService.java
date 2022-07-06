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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.rest.DocumentMetadataService;
import com.logicaldoc.webservice.soap.endpoint.SoapDocumentMetadataService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
	//@ApiOperation(value = "Save attribute options", notes = "Updates the options for the given attribute")
	@Operation(summary = "Save attribute options", description = "Updates the options for the given attribute")
	public void setAttributeOptions(@QueryParam("setId") long setId, @QueryParam("attribute") String attribute, @QueryParam("values") String[] values) throws Exception {
		String sid = validateSession();
		super.setAttributeOptions(sid, setId, attribute, values);
	}

	@Override
	@POST
	@Path("/storeAttributeSet")
	//@ApiOperation(value = "Create/Update an attribute set", notes = "Create/Update an attribute set. You can completely customize the attribute set through a value object containing the attribute set's metadata")
	@Operation(summary = "Create/Update an attribute set", description = "Create/Update an attribute set. You can completely customize the attribute set through a value object containing the attribute set's metadata")	
	public long storeAttributeSet(WSAttributeSet attributeSet) throws Exception {
		String sid = validateSession();
		return super.storeAttributeSet(sid, attributeSet);
	}

	@Override
	@POST
	@Path("/storeTemplate")
	//@ApiOperation(value = "Create/Update a template", notes = "Create/Update a template. You can completely customize the template through a value object")
	@Operation(summary = "Create/Update a template", description = "Create/Update a template. You can completely customize the template through a value object")	
	public long storeTemplate(WSTemplate template) throws Exception {
		String sid = validateSession();
		return super.storeTemplate(sid, template);
	}

	@Override
	@GET
	@Path("/getAttributeSetById")
	//@ApiOperation(value = "Gets attribute set's metadata", notes = "Gets attribute set's metadata. Returns a value object containing the attribute set's metadata")
	@Operation(summary = "Gets attribute set's metadata", description = "Gets attribute set's metadata. Returns a value object containing the attribute set's metadata")		
	public WSAttributeSet getAttributeSetById(@Parameter(description = "Attribute set identifier (ID)") @QueryParam("setId") long setId) throws Exception {
		String sid = validateSession();
		return super.getAttributeSetById(sid, setId);
	}

	@Override
	@GET
	@Path("/getAttributeSet")
	//@ApiOperation(value = "Gets an attribute set by name", notes = "Gets an attribute set by name. Returns a value object containing the attribute set's metadata")
	@Operation(summary = "Gets an attribute set by name", description = "Gets an attribute set by name. Returns a value object containing the attribute set's metadata")		
	public WSAttributeSet getAttributeSet(@Parameter(description = "The attribute set's name") @QueryParam("name") String name) throws Exception {
		String sid = validateSession();
		return super.getAttributeSet(sid, name);
	}

	@Override
	@GET
	@Path("/getTemplate")
	//@ApiOperation(value = "Gets template's metadata", notes = "Gets an existing template by it's name")
	@Operation(summary = "Gets template's metadata", description = "Gets an existing template by its name")	
	public WSTemplate getTemplate(@Parameter(description = "The template name") @QueryParam("name") String name) throws Exception {
		String sid = validateSession();
		return super.getTemplate(sid, name);
	}

	@Override
	@GET
	@Path("/getTemplateById")
	//@ApiOperation(value = "Gets template's metadata", notes = "Gets an existing template by it's identifier")
	@Operation(summary = "Gets template's metadata", description = "Gets an existing template by its identifier")	
	public WSTemplate getTemplateById(@Parameter(description = "The template identifier (ID)") @QueryParam("templateId") long templateId) throws Exception {
		String sid = validateSession();
		return super.getTemplateById(sid, templateId);
	}
	
	@Override
	@GET
	@Path("/getAttributeOptions")
	//@ApiOperation(value = "Retrieves the options for the given attribute", notes = "Returns the list of all the attribute's options")
	@Operation(summary = "Retrieves the options for the given attribute", description = "Returns the list of all the attribute's options")
	public String[] getAttributeOptions(@Parameter(description = "Attribute set identifier (ID)") @QueryParam("setId") long setId, @Parameter(description = "The attribute's name")  @QueryParam("attribute") String attribute) throws Exception {
		String sid = validateSession();
		return super.getAttributeOptions(sid, setId, attribute);
	}

	@Override
	@GET
	@Path("/listAttributeSets")
	//@ApiOperation(value = "Lists the attribute sets", notes = "Gets metadata of all existing attribute sets")	
	@Operation(summary = "Lists the attribute sets", description = "Gets metadata of all existing attribute sets")	
	public WSAttributeSet[] listAttributeSets() throws Exception {
		String sid = validateSession();
		return super.listAttributeSets(sid);
	}

	
	
	@Override
	@DELETE
	@Path("/deleteAttributeSet")
	//@ApiOperation(value = "Deletes an attribute set", notes = "Deletes an existing attribute set with the given identifier")
	@Operation(summary = "Deletes an attribute set", description = "GDeletes an existing attribute set with the given identifier")		
	public void deleteAttributeSet(@Parameter(description = "Attribute set identifier (ID)") @QueryParam("setId") long setId) throws Exception {
		String sid = validateSession();
		super.deleteAttributeSet(sid, setId);
	}

	@Override
	@DELETE
	@Path("/deleteTemplate")	
	//@ApiOperation(value = "Deletes a template", notes = "Deletes an existing template with the given identifier")
	@Operation(summary = "Deletes a template", description = "Deletes an existing template with the given identifier")	
	public void deleteTemplate(@Parameter(description = "A template ID") @QueryParam("templateId") long templateId) throws Exception {
		String sid = validateSession();
		super.deleteTemplate(sid, templateId);		
	}

	@Override
	@GET
	@Path("/listTemplates")
	//@ApiOperation(value = "Lists all the templates", notes = "Gets metadata of all existing templates")
	@Operation(summary = "Lists all the templates", description = "Gets metadata of all existing templates")		
	public WSTemplate[] listTemplates() throws Exception {
		String sid = validateSession();
		return super.listTemplates(sid);	
	}

	@Override
	@POST
	@Path("/setAttributeOptionsPOST")
	@Operation(summary = "Save attribute options with a POST method", description = "Saves the options for the given attribute with a POST method. This is useful for very large lists of values")
	public void setAttributeOptionsPOST(@FormParam("setId") long setId, @FormParam("attribute") String attribute, @FormParam("values") String[] values) throws Exception {
		String sid = validateSession();
		super.setAttributeOptions(sid, setId, attribute, values);
	}
}
