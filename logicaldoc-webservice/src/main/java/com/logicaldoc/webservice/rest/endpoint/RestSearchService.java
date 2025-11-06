package com.logicaldoc.webservice.rest.endpoint;

import java.util.List;

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
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

@Path("/")
@Tag(name = "search")
@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestSearchService extends SoapSearchService implements SearchService {

	@POST
	@Path("/find")
	@Operation(summary = "Search documents", description = "Runs a search on the server")
	public WSSearchResult find(
			@Parameter(description = "Search options", required = true, example = "{\"maxHits\":50,\"expression\":\"document management system\",\"expressionLanguage\":\"en\",\"language\":\"en\"}", schema = @Schema(implementation = WSSearchOptions.class))
			WSSearchOptions opt)
			throws AuthenticationException, PersistenceException, WebserviceException, SearchException {
		String sid = validateSessionREST();
		return super.find(sid, opt);
	}

	@Override
	@GET
	@Path("/findByFilename")
	@Operation(summary = "Search documents by Filename", description = "Finds authorized documents for the current user containing the given filename (like operator is used)")
	public List<WSDocument> findByFilename(@Parameter(description = "Filename of the document", required = true)
	@QueryParam("filename")
	String filename) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.findByFilename(sid, filename);
	}

	@Override
	@GET
	@Path("/findFolders")
	@Operation(summary = "Search folders by name", description = "Finds authorized folders for the current user containing the given name (like operator is used)")
	public List<WSFolder> findFolders(@Parameter(description = "Name of the folder", required = true)
	@QueryParam("name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.findFolders(sid, name);
	}
}