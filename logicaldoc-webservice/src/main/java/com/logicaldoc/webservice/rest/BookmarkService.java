package com.logicaldoc.webservice.rest;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.logicaldoc.webservice.model.WSBookmark;

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
	public WSBookmark saveBookmark(WSBookmark bookmark) throws Exception;

	@GET
	@Path("/bookmarkDocument")
	public WSBookmark bookmarkDocument(@QueryParam("docId") long docId) throws Exception;

	@GET
	@Path("/bookmarkFolder")
	public WSBookmark bookmarkFolder(@QueryParam("folderId") long folderId) throws Exception;

	@GET
	@Path("/getBookmarks")
	public WSBookmark[] getBookmarks() throws Exception;

	@DELETE
	@Path("/deleteBookmark")
	public void deleteBookmark(@QueryParam("bookmarkId") long bookmarkId) throws Exception;

	@DELETE
	@Path("/unbookmarkDocument")
	public void unbookmarkDocument(@QueryParam("docId") long docId) throws Exception;

	@DELETE
	@Path("/unbookmarkFolder")
	public void unbookmarkFolder(@QueryParam("folderId") long folderId) throws Exception;
}