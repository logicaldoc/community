package com.logicaldoc.webservice.soap;

import java.util.List;

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebResult;
import jakarta.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.doc.WSDoc;
import com.logicaldoc.webservice.model.WSBookmark;

/**
 * Bookmark Web Service definition interface
 * 
 * Marco Meschieri - LogicalDOC
 * 
 * @since 7.6.3
 */
@WSDoc(description = "Bookmarks management and search")
@WebService(name = "Bookmark", serviceName = "Bookmark", targetNamespace = "http://ws.logicaldoc.com")
public interface BookmarkService {

	@WebMethod(action = "saveBookmark")
	@WebResult(name = "bookmark")
	@WSDoc(description = "crates or updates a bookmark")
	public WSBookmark saveBookmark(@WebParam(name = "sid")
	String sid, @WebParam(name = "bookmark")
	WSBookmark bookmark) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException,
			UnexistingResourceException;

	@WebMethod(action = "bookmarkDocument")
	@WebResult(name = "bookmark")
	@WSDoc(description = "bookmarks a document")
	public WSBookmark bookmarkDocument(@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException, PermissionException,
			UnexistingResourceException;

	@WebMethod(action = "bookmarkFolder")
	@WebResult(name = "bookmark")
	@WSDoc(description = "bookmarks a folder")
	public WSBookmark bookmarkFolder(@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	/**
	 * Retrieves all the bookmarks of the current user
	 * 
	 * @param sid identifier of the session or an API Key
	 * 
	 * @return list of bookmarks
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 */
	@WebMethod(action = "getBookmarks")
	@WebResult(name = "bookmark")
	@WSDoc(description = "retrieves all the bookmarks of the current user")
	public List<WSBookmark> getBookmarks(@WebParam(name = "sid")
	String sid) throws AuthenticationException, WebserviceException, PersistenceException;

	@WebMethod(action = "deleteBookmark")
	@WSDoc(description = "deletes a bookmark")
	public void deleteBookmark(@WebParam(name = "sid")
	String sid, @WebParam(name = "bookmarkId")
	long bookmarkId) throws AuthenticationException, WebserviceException, PersistenceException;

	@WebMethod(action = "unbookmarkDocument")
	@WSDoc(description = "unbookmarks a document")
	public void unbookmarkDocument(@WebParam(name = "sid")
	String sid, @WebParam(name = "docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException;

	@WebMethod(action = "unbookmarkFolder")
	@WSDoc(description = "unbookmarks a folder")
	public void unbookmarkFolder(@WebParam(name = "sid")
	String sid, @WebParam(name = "folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException;
}