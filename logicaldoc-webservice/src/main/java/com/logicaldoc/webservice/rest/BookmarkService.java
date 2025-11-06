package com.logicaldoc.webservice.rest;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

/**
 * Bookmark Web Service definition interface for REST
 * 
 * Marco Meschieri - LogicalDOC
 * 
 * @since 7.6.3
 */
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public interface BookmarkService {

	@POST
	@Path("/saveBookmark")
	public WSBookmark saveBookmark(WSBookmark bookmark)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException;

	@GET
	@Path("/bookmarkDocument")
	public WSBookmark bookmarkDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException;

	@GET
	@Path("/bookmarkFolder")
	public WSBookmark bookmarkFolder(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@GET
	@Path("/getBookmarks")
	public List<WSBookmark> getBookmarks()
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@DELETE
	@Path("/deleteBookmark")
	public void deleteBookmark(@QueryParam("bookmarkId")
	long bookmarkId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@DELETE
	@Path("/unbookmarkDocument")
	public void unbookmarkDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;

	@DELETE
	@Path("/unbookmarkFolder")
	public void unbookmarkFolder(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException;
}