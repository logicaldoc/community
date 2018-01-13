package com.logicaldoc.webservice.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

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

	@WebMethod
	@WebResult(name = "bookmark")
	@WSDoc(description = "crates or updates a bookmark")
	public WSBookmark saveBookmark(@WebParam(name = "sid") String sid, @WebParam(name = "bookmark") WSBookmark bookmark)
			throws Exception;

	@WebMethod
	@WebResult(name = "bookmark")
	@WSDoc(description = "bookmarks a document")
	public WSBookmark bookmarkDocument(@WebParam(name = "sid") String sid, @WebParam(name = "docId") long docId)
			throws Exception;

	@WebMethod
	@WebResult(name = "bookmark")
	@WSDoc(description = "bookmarks a folder")
	public WSBookmark bookmarkFolder(@WebParam(name = "sid") String sid, @WebParam(name = "folderId") long folderId)
			throws Exception;

	/**
	 * Retrieves all the bookmarks of the current user.
	 */
	@WebMethod
	@WebResult(name = "bookmark")
	@WSDoc(description = "retrieves all the bookmarks of the current user")
	public WSBookmark[] getBookmarks(@WebParam(name = "sid") String sid) throws Exception;

	@WebMethod
	@WSDoc(description = "deletes a bookmark")
	public void deleteBookmark(@WebParam(name = "sid") String sid, @WebParam(name = "bookmarkId") long bookmarkId)
			throws Exception;

	@WebMethod
	@WSDoc(description = "unbookmarks a document")
	public void unbookmarkDocument(@WebParam(name = "sid") String sid, @WebParam(name = "docId") long docId)
			throws Exception;

	@WebMethod
	@WSDoc(description = "unbookmarks a folder")
	public void unbookmarkFolder(@WebParam(name = "sid") String sid, @WebParam(name = "folderId") long folderId)
			throws Exception;
}
