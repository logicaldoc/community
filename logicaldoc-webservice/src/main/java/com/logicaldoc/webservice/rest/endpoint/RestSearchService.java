package com.logicaldoc.webservice.rest.endpoint;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;
import com.logicaldoc.webservice.rest.SearchService;
import com.logicaldoc.webservice.soap.endpoint.SoapSearchService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "search")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSearchService extends SoapSearchService implements SearchService {

	protected static Logger log = LoggerFactory.getLogger(RestSearchService.class);

	@POST
    @Path("/find")
	//@ApiOperation(value = "Search documents", notes = "Runs a search on the server")
	@Operation(summary = "Search documents", description = "Runs a search on the server")
	public WSSearchResult find(
			@Parameter(
					description = "Search options", required = true,
					example = "{\"maxHits\":50,\"expression\":\"document management system\",\"expressionLanguage\":\"en\",\"language\":\"en\"}",
					schema = @Schema(implementation = WSSearchOptions.class))
			WSSearchOptions opt) throws AuthenticationException, PersistenceException, WebserviceException, SearchException 
			{
		String sid = validateSessionREST();
		return super.find(sid, opt);
	}

	@Override
	@GET
	@Path("/findByFilename")
	//@ApiOperation(value = "Search documents by Filename", notes = "Finds authorized documents for the current user containing the given filename (like operator is used)")
	@Operation(summary = "Search documents by Filename", description = "Finds authorized documents for the current user containing the given filename (like operator is used)")
	public WSDocument[] findByFilename(@Parameter(description = "Filename of the document", required = true) @QueryParam("filename") String filename) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.findByFilename(sid, filename);
	}

	@Override
	@GET
	@Path("/findFolders")
	//@ApiOperation(value = "Search folders by name", notes = "Finds authorized folders for the current user containing the given name (like operator is used)")
	@Operation(summary = "Search folders by name", description = "Finds authorized folders for the current user containing the given name (like operator is used)")
	public WSFolder[] findFolders(@Parameter(description = "Name of the folder", required = true) @QueryParam("name") String name) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.findFolders(sid, name);
	}
}
