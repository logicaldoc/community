package com.logicaldoc.webservice.rest.client;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.rest.BookmarkService;

public class RestBookmarkClient extends AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(RestBookmarkClient.class);

	private BookmarkService proxy = null;

	public RestBookmarkClient(String endpoint, String username, String password) {
		this(endpoint, username, password, -1);
	}

	public RestBookmarkClient(String endpoint, String username, String password, int timeout) {
		super(endpoint, username, password, timeout);

		JacksonJsonProvider provider = new JacksonJsonProvider();

		if ((username == null) || (password == null)) {
			proxy = JAXRSClientFactory.create(endpoint, BookmarkService.class, Arrays.asList(provider));
		} else {
			proxy = JAXRSClientFactory.create(endpoint, BookmarkService.class, Arrays.asList(provider), username,
					password, null);
		}

		if (timeout > 0) {
			HTTPConduit conduit = WebClient.getConfig(proxy).getHttpConduit();
			HTTPClientPolicy policy = new HTTPClientPolicy();
			policy.setReceiveTimeout(timeout);
			conduit.setClient(policy);
		}
	}

	public WSBookmark saveBookmark(WSBookmark bookmark) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.saveBookmark(bookmark);
	}

	public WSBookmark bookmarkDocument(@QueryParam("docId") long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.bookmarkDocument(docId);
	}

	@GET
	@Path("/bookmarkFolder")
	public WSBookmark bookmarkFolder(@QueryParam("folderId") long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.bookmarkFolder(folderId);
	}

	/**
	 * Retrieves all the bookmarks of the current user
	 * 
	 * @return list of bookmarks
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws WebserviceException Error in the webservice
	 * @throws AuthenticationException Invalid session
	 * @throws PermissionException The user does not have the required
	 *         permission
	 */
	@GET
	@Path("/getBookmarks")
	public List<WSBookmark> getBookmarks() throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		return proxy.getBookmarks();
	}

	@DELETE
	@Path("/deleteBookmark")
	public void deleteBookmark(@QueryParam("bookmarkId") long bookmarkId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.deleteBookmark(bookmarkId);
	}

	@DELETE
	@Path("/unbookmarkDocument")
	public void unbookmarkDocument(@QueryParam("docId") long docId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.unbookmarkDocument(docId);
	}

	@DELETE
	@Path("/unbookmarkFolder")
	public void unbookmarkFolder(@QueryParam("folderId") long folderId) throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		WebClient.client(proxy).type(MediaType.APPLICATION_JSON);
		WebClient.client(proxy).accept(MediaType.APPLICATION_JSON);
		proxy.unbookmarkFolder(folderId);
	}
}