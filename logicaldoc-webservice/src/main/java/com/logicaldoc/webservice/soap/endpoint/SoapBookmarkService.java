package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSBookmark;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.BookmarkService;

/**
 * Bookmark Web Service Implementation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class SoapBookmarkService extends AbstractService implements BookmarkService {
	protected static Logger log = LoggerFactory.getLogger(SoapBookmarkService.class);

	@Override
	public WSBookmark saveBookmark(String sid, WSBookmark bookmark) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);
		checkObjectAvailability(sid, bookmark);
		return storeBookmark(bookmark, user);
	}

	private void checkObjectAvailability(String sid, WSBookmark bookmark) throws AuthenticationException,
			PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
		if (bookmark.getType() == Bookmark.TYPE_DOCUMENT) {
			checkDocumentAvailable(sid, bookmark.getTargetId());
		} else {
			checkFolderAvailable(sid, bookmark.getTargetId());
		}
	}

	private WSFolder checkFolderAvailable(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		SoapFolderService folderService = new SoapFolderService();
		folderService.setValidateSession(isValidateSession());
		WSFolder folder = folderService.getFolder(sid, folderId);
		if (folder == null)
			throw new PermissionException("Folder " + folderId + " not found or not accessible");
		else
			return folder;
	}

	private WSDocument checkDocumentAvailable(String sid, long docId) throws PermissionException,
			AuthenticationException, WebserviceException, PersistenceException, UnexistingResourceException {
		SoapDocumentService docService = new SoapDocumentService();
		docService.setValidateSession(isValidateSession());
		return docService.getDocument(sid, docId);
	}

	private WSBookmark storeBookmark(WSBookmark bookmark, User user) throws PersistenceException {
		BookmarkDAO bDao = Context.get(BookmarkDAO.class);

		Bookmark bmark = null;
		if (bookmark.getType() == Bookmark.TYPE_DOCUMENT)
			bmark = bDao.findByUserIdAndDocId(user.getId(), bookmark.getTargetId());
		else
			bmark = bDao.findByUserIdAndFolderId(user.getId(), bookmark.getTargetId());

		if (bmark == null)
			bmark = new Bookmark();
		bmark.setDescription(bookmark.getDescription());
		bmark.setTitle(bookmark.getTitle());
		bmark.setFileType(bookmark.getFileType());
		bmark.setPosition(bookmark.getPosition());
		bmark.setTenantId(user.getTenantId());
		bmark.setTargetId(bookmark.getTargetId());
		bmark.setType(bookmark.getType());
		bmark.setUserId(user.getId());

		bDao.store(bmark);

		return WSBookmark.fromBookmark(bmark);
	}

	@Override
	public WSBookmark bookmarkDocument(String sid, long docId) throws AuthenticationException, WebserviceException,
			PersistenceException, PermissionException, UnexistingResourceException {
		User user = validateSession(sid);

		WSDocument doc = checkDocumentAvailable(sid, docId);

		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		Bookmark bmark = bDao.findByUserIdAndDocId(user.getId(), docId);
		if (bmark == null) {
			bmark = new Bookmark();
			bmark.setType(Bookmark.TYPE_DOCUMENT);
			bmark.setFileType(doc.getFileName());
			bmark.setFileType(doc.getType());
			bmark.setUserId(user.getId());
			bmark.setTargetId(docId);
			bDao.store(bmark);
		}

		return WSBookmark.fromBookmark(bmark);
	}

	@Override
	public WSBookmark bookmarkFolder(String sid, long folderId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		checkFolderAvailable(sid, folderId);
		User user = validateSession(sid);

		WSFolder folder = checkFolderAvailable(sid, folderId);

		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		Bookmark bmark = bDao.findByUserIdAndDocId(user.getId(), folderId);
		if (bmark == null) {
			bmark = new Bookmark();
			bmark.setType(Bookmark.TYPE_DOCUMENT);
			bmark.setTitle(folder.getName());
			bmark.setFileType("folder");
			bmark.setUserId(user.getId());
			bmark.setTargetId(folderId);
			bDao.store(bmark);
		}

		return WSBookmark.fromBookmark(bmark);
	}

	@Override
	public List<WSBookmark> getBookmarks(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		List<Bookmark> list = bDao.findByUserId(user.getId());
		List<WSBookmark> wsBookmarks = new ArrayList<>();
		for (Bookmark bookmark : list)
			wsBookmarks.add(WSBookmark.fromBookmark(bookmark));
		return wsBookmarks;
	}

	@Override
	public void deleteBookmark(String sid, long bookmarkId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		Bookmark bookmark = bDao.findById(bookmarkId);
		if (bookmark == null || bookmark.getUserId() != user.getId())
			throw new WebserviceException("Bookmark " + bookmarkId + " not found or not accessible");
		bDao.delete(bookmarkId);
	}

	@Override
	public void unbookmarkDocument(String sid, long docId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		Bookmark bookmark = bDao.findByUserIdAndDocId(user.getId(), docId);
		if (bookmark != null)
			deleteBookmark(sid, bookmark.getId());
	}

	@Override
	public void unbookmarkFolder(String sid, long folderId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		User user = validateSession(sid);
		BookmarkDAO bDao = Context.get(BookmarkDAO.class);
		Bookmark bookmark = bDao.findByUserIdAndFolderId(user.getId(), folderId);
		if (bookmark != null)
			deleteBookmark(sid, bookmark.getId());
	}
}