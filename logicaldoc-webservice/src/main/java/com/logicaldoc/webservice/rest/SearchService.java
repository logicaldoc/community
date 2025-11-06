package com.logicaldoc.webservice.rest;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Consumes({ MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface SearchService {

	@POST
	@Path("/find")
	WSSearchResult find(WSSearchOptions opt)
			throws AuthenticationException, PersistenceException, WebserviceException, SearchException;

	/**
	 * Finds authorized documents for the current user the given filename (like
	 * operator is used)
	 * 
	 * @param filename Filename of the document
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/findByFilename")
	public List<WSDocument> findByFilename(String filename)
			throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Finds authorized folders for the current user containing the given name
	 * (like operator is used)
	 * 
	 * @param name Name of the folder
	 * 
	 * @return Collection of found folders
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@GET
	@Path("/findFolders")
	public List<WSFolder> findFolders(String name)
			throws AuthenticationException, WebserviceException, PersistenceException;

}