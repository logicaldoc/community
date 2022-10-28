package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

import javax.jws.WebService;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.soap.BookmarkService;

/**
 * Bookmark Web Service client.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@WebService(name = "Bookmark", serviceName = "Bookmark")
public class SoapBookmarkClient extends SoapClient<BookmarkService> implements BookmarkService {

	public SoapBookmarkClient(String endpoint, int gzipThreshold, boolean log, int timeout) throws IOException {
		super(endpoint, BookmarkService.class, gzipThreshold, log, timeout);
	}

	public SoapBookmarkClient(String endpoint) throws IOException {
		super(endpoint, BookmarkService.class, -1, true, -1);
	}

	@Override
	public WSBookmark saveBookmark(String sid, WSBookmark bookmark)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.saveBookmark(sid, bookmark);
	}

	@Override
	public WSBookmark bookmarkDocument(String sid, long docId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.bookmarkDocument(sid, docId);
	}

	@Override
	public WSBookmark bookmarkFolder(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.bookmarkFolder(sid, folderId);
	}

	@Override
	public WSBookmark[] getBookmarks(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getBookmarks(sid);
	}

	@Override
	public void deleteBookmark(String sid, long bookmarkId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.deleteBookmark(sid, bookmarkId);
	}

	@Override
	public void unbookmarkDocument(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.unbookmarkDocument(sid, docId);
	}

	@Override
	public void unbookmarkFolder(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.unbookmarkFolder(sid, folderId);
	}
}