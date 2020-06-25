package com.logicaldoc.webservice.rest.endpoint;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.Example;
import io.swagger.annotations.ExampleProperty;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.SearchService;
import com.logicaldoc.webservice.soap.endpoint.SoapSearchService;

@Path("/")
@Api(value = "search")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSearchService extends SoapSearchService implements SearchService {

	protected static Logger log = LoggerFactory.getLogger(RestSearchService.class);

	@POST
	@Path("/find")
	@ApiOperation(value = "Search documents", notes = "Runs a search on the server")
	public WSSearchResult find(
			@ApiParam(value = "Search options", required = true, examples = @Example(value = { @ExampleProperty(value = "{\"maxHits\":50,\"expression\":\"document management system\",\"expressionLanguage\":\"en\",\"language\":\"en\"}") })) WSSearchOptions opt)
			throws Exception {
		String sid = validateSession();
		return super.find(sid, opt);
	}

	@Override
	@GET
	@Path("/findByFilename")
	@ApiOperation(value = "Search documents by Filename", notes = "Finds authorized documents for the current user containing the given filename (like operator is used)")
	public WSDocument[] findByFilename(@QueryParam("filename") @ApiParam(value = "Filename of the document", required = true) String filename) throws Exception {
		String sid = validateSession();
		return super.findByFilename(sid, filename);
	}

	@Override
	@GET
	@Path("/findFolders")
	@ApiOperation(value = "Search folders by name", notes = "Finds authorized folders for the current user containing the given name (like operator is used)")	
	public WSFolder[] findFolders(@QueryParam("name") @ApiParam(value = "Name of the folder", required = true) String name) throws Exception {
		String sid = validateSession();
		return super.findFolders(sid, name);
	}
}
