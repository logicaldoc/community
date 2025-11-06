package com.logicaldoc.webservice.rest.endpoint;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.rest.BookmarkService;
import com.logicaldoc.webservice.soap.endpoint.SoapBookmarkService;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

@Path("/")
@Tag(name = "bookmark")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestBookmarkService extends SoapBookmarkService implements BookmarkService {

	@POST
	@Path("/saveBookmark")
	public WSBookmark saveBookmark(WSBookmark bookmark) throws AuthenticationException, PermissionException,
			WebserviceException, PersistenceException, UnexistingResourceException {
		String sid = validateSessionREST();
		return super.saveBookmark(sid, bookmark);
	}

	@GET
	@Path("/bookmarkDocument")
	public WSBookmark bookmarkDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException,
			UnexistingResourceException {
		String sid = validateSessionREST();
		return super.bookmarkDocument(sid, docId);
	}

	@GET
	@Path("/bookmarkFolder")
	public WSBookmark bookmarkFolder(@QueryParam("folderId")
	long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.bookmarkFolder(sid, folderId);
	}

	@GET
	@Path("/getBookmarks")
	public List<WSBookmark> getBookmarks() throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		return super.getBookmarks(sid);
	}

	@DELETE
	@Path("/deleteBookmark")
	public void deleteBookmark(@QueryParam("bookmarkId")
	long bookmarkId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.deleteBookmark(sid, bookmarkId);
	}

	@DELETE
	@Path("/unbookmarkDocument")
	public void unbookmarkDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.unbookmarkDocument(sid, docId);
	}

	@DELETE
	@Path("/unbookmarkFolder")
	public void unbookmarkFolder(@QueryParam("folderId")
	long folderId) throws AuthenticationException, WebserviceException, PersistenceException {
		String sid = validateSessionREST();
		super.unbookmarkFolder(sid, folderId);
	}
}