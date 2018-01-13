package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

import javax.jws.WebService;

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
	public WSBookmark saveBookmark(String sid, WSBookmark bookmark) throws Exception {
		return client.saveBookmark(sid, bookmark);
	}

	@Override
	public WSBookmark bookmarkDocument(String sid, long docId) throws Exception {
		return client.bookmarkDocument(sid, docId);
	}

	@Override
	public WSBookmark bookmarkFolder(String sid, long folderId) throws Exception {
		return client.bookmarkFolder(sid, folderId);
	}

	@Override
	public WSBookmark[] getBookmarks(String sid) throws Exception {
		return client.getBookmarks(sid);
	}

	@Override
	public void deleteBookmark(String sid, long bookmarkId) throws Exception {
		client.deleteBookmark(sid, bookmarkId);
	}

	@Override
	public void unbookmarkDocument(String sid, long docId) throws Exception {
		client.unbookmarkDocument(sid, docId);
	}

	@Override
	public void unbookmarkFolder(String sid, long folderId) throws Exception {
		client.unbookmarkFolder(sid, folderId);
	}
}