package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSSearchOptions;
import com.logicaldoc.webservice.model.WSSearchResult;

/**
 * Search Web Service definition interface
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@WSDoc(description = "search operations")
@WebService(name = "Search", serviceName = "Search", targetNamespace = "http://ws.logicaldoc.com")
public interface SearchService {
	/**
	 * Performs a search by the search options.
	 * 
	 * @param sid identifier of the session
	 * @param options Search options
	 * 
	 * @return The search result
	 * 
	 * @throws SearchException Eror in the search
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@WebMethod
	@WebResult(name = "searchResult")
	@WSDoc(description = "performs a search by the search options")
	public WSSearchResult find(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "options")
	WSSearchOptions options) throws PersistenceException, AuthenticationException, WebserviceException, SearchException;

	/**
	 * Finds authorized documents for the current user the given filename (like
	 * operator is used)
	 * 
	 * @param sid identifier of the session
	 * @param filename Filename of the document
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod
	@WebResult(name = "document")
	@WSDoc(description = "finds authorized documents for the current user with the given filename (like '%' operator is used)")
	public WSDocument[] findByFilename(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "filename")
	String filename) throws AuthenticationException, WebserviceException, PersistenceException;

	/**
	 * Finds authorized folders for the current user containing the given name
	 * (like operator is used)
	 * 
	 * @param sid identifier of the session
	 * @param name Name of the folder
	 * 
	 * @return Collection of found folders
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod
	@WebResult(name = "folder")
	@WSDoc(description = "finds authorized folders for the current user containing the given name (like '%' operator is used)")
	public WSFolder[] findFolders(@WSDoc(description = "identifier of the session", required = true)
	@WebParam(name = "sid")
	String sid, @WebParam(name = "name")
	String name) throws AuthenticationException, WebserviceException, PersistenceException;
}