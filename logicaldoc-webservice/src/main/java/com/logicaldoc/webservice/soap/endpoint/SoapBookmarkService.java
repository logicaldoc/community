package com.logicaldoc.webservice.soap.endpoint;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.core.security.authorization.UnexistingResourceException;
import com.logicaldoc.core.security.user.User;
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

    @Override
    public WSBookmark saveBookmark(String sid, WSBookmark bookmark) throws AuthenticationException, WebserviceException,
            PersistenceException, PermissionException, UnexistingResourceException {
        User user = validateSession(sid);
        checkObjectAvailability(sid, bookmark);
        return storeBookmark(bookmark, user);
    }

    private void checkObjectAvailability(String sid, WSBookmark bookmark) throws AuthenticationException,
            PermissionException, WebserviceException, PersistenceException, UnexistingResourceException {
        if (bookmark.getType() == Bookmark.Type.DOCUMENT.ordinal()) {
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
            throw new PermissionException("Folder %d not found or not accessible".formatted(folderId));
        else
            return folder;
    }

    private WSDocument checkDocumentAvailable(String sid, long docId) throws PermissionException,
            AuthenticationException, WebserviceException, PersistenceException, UnexistingResourceException {
        SoapDocumentService docService = new SoapDocumentService();
        docService.setValidateSession(isValidateSession());
        return docService.getDocument(sid, docId);
    }

    private WSBookmark storeBookmark(WSBookmark wsBookmark, User user) throws PersistenceException {
        BookmarkDAO dao = BookmarkDAO.get();

        Bookmark bookmark = null;
        if (wsBookmark.getType() == Bookmark.Type.DOCUMENT.ordinal())
            bookmark = dao.findByUserIdAndDocId(user.getId(), wsBookmark.getTargetId());
        else
            bookmark = dao.findByUserIdAndFolderId(user.getId(), wsBookmark.getTargetId());

        if (bookmark == null)
            bookmark = new Bookmark();
        bookmark.setDescription(wsBookmark.getDescription());
        bookmark.setTitle(wsBookmark.getTitle());
        bookmark.setFileType(wsBookmark.getFileType());
        bookmark.setPosition(wsBookmark.getPosition());
        bookmark.setTenantId(user.getTenantId());
        bookmark.setTargetId(wsBookmark.getTargetId());
        bookmark.setType(Bookmark.Type.values()[wsBookmark.getType()]);
        bookmark.setUserId(user.getId());

        dao.store(bookmark);

        return WSBookmark.fromBookmark(bookmark);
    }

    @Override
    public WSBookmark bookmarkDocument(String sid, long docId) throws AuthenticationException, WebserviceException,
            PersistenceException, PermissionException, UnexistingResourceException {
        User user = validateSession(sid);

        WSDocument doc = checkDocumentAvailable(sid, docId);

        BookmarkDAO bDao = BookmarkDAO.get();
        Bookmark bmark = bDao.findByUserIdAndDocId(user.getId(), docId);
        if (bmark == null) {
            bmark = new Bookmark();
            bmark.setType(Bookmark.Type.DOCUMENT);
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

        BookmarkDAO dao = BookmarkDAO.get();
        Bookmark bookmark = dao.findByUserIdAndDocId(user.getId(), folderId);
        if (bookmark == null) {
            bookmark = new Bookmark();
            bookmark.setType(Bookmark.Type.DOCUMENT);
            bookmark.setTitle(folder.getName());
            bookmark.setFileType("folder");
            bookmark.setUserId(user.getId());
            bookmark.setTargetId(folderId);
            dao.store(bookmark);
        }

        return WSBookmark.fromBookmark(bookmark);
    }

    @Override
    public List<WSBookmark> getBookmarks(String sid)
            throws AuthenticationException, WebserviceException, PersistenceException {
        User user = validateSession(sid);
        List<Bookmark> list = BookmarkDAO.get().findByUserId(user.getId());
        List<WSBookmark> wsBookmarks = new ArrayList<>();
        for (Bookmark bookmark : list)
            wsBookmarks.add(WSBookmark.fromBookmark(bookmark));
        return wsBookmarks;
    }

    @Override
    public void deleteBookmark(String sid, long bookmarkId)
            throws AuthenticationException, WebserviceException, PersistenceException {
        User user = validateSession(sid);
        Bookmark bookmark = BookmarkDAO.get().findById(bookmarkId);
        if (bookmark == null || bookmark.getUserId() != user.getId())
            throw new WebserviceException("Bookmark " + bookmarkId + " not found or not accessible");
        BookmarkDAO.get().delete(bookmarkId);
    }

    @Override
    public void unbookmarkDocument(String sid, long docId)
            throws AuthenticationException, WebserviceException, PersistenceException {
        User user = validateSession(sid);
        Bookmark bookmark = BookmarkDAO.get().findByUserIdAndDocId(user.getId(), docId);
        if (bookmark != null)
            deleteBookmark(sid, bookmark.getId());
    }

    @Override
    public void unbookmarkFolder(String sid, long folderId)
            throws AuthenticationException, WebserviceException, PersistenceException {
        User user = validateSession(sid);
        Bookmark bookmark = BookmarkDAO.get().findByUserIdAndFolderId(user.getId(), folderId);
        if (bookmark != null)
            deleteBookmark(sid, bookmark.getId());
    }
}