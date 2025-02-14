package com.logicaldoc.webservice.rest.endpoint;

import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.rest.BookmarkService;
import com.logicaldoc.webservice.soap.endpoint.SoapBookmarkService;

import io.swagger.v3.oas.annotations.tags.Tag;

@Path("/")
@Tag(name = "bookmark")
@Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
@Produces({ MediaType.APPLICATION_JSON })
public class RestBookmarkService extends SoapBookmarkService implements BookmarkService {
	private static final Logger log = LoggerFactory.getLogger(RestBookmarkService.class);

	@POST
	@Path("/saveBookmark")
	public WSBookmark saveBookmark(WSBookmark bookmark) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		String sid = validateSessionREST();
		return super.saveBookmark(sid, bookmark);
	}

	@GET
	@Path("/bookmarkDocument")
	public WSBookmark bookmarkDocument(@QueryParam("docId")
	long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
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