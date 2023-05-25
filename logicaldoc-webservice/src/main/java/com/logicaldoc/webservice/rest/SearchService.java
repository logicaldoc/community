package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

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
	public WSDocument[] findByFilename(String filename)
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
	public WSFolder[] findFolders(String name)
			throws AuthenticationException, WebserviceException, PersistenceException;

}