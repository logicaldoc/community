package com.logicaldoc.gui.frontend.client.services;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;

public interface DocumentServiceAsync {

	void getVersionsById(long id1, long id2, AsyncCallback<List<GUIVersion>> callback);

	void getById(long docId, AsyncCallback<GUIDocument> callback);

	void isPasswordProtected(long docId, AsyncCallback<Boolean> callback);

	void save(GUIDocument document, AsyncCallback<GUIDocument> callback);

	void sendAsEmail(GUIEmail email, String locale, AsyncCallback<String> callback);

	void updateLink(long id, String type, AsyncCallback<Void> callback);

	void deleteLinks(List<Long> ids, AsyncCallback<Void> callback);

	void delete(List<Long> ids, AsyncCallback<Void> callback);

	void destroyDocuments(List<Long> ids, AsyncCallback<Void> callback);

	void makeImmutable(List<Long> ids, String comment, AsyncCallback<Void> callback);

	void lock(List<Long> ids, String comment, AsyncCallback<Void> callback);

	void unlock(List<Long> ids, AsyncCallback<Void> callback);

	void addDocuments(String language, long folderId, boolean importZip, String charset, boolean immediateIndexing,
			Long templateId, AsyncCallback<List<GUIDocument>> callback);

	void checkout(List<Long> docIds, AsyncCallback<Void> callback);

	void checkin(GUIDocument document, boolean major, AsyncCallback<GUIDocument> callback);

	void linkDocuments(List<Long> inDocIds, List<Long> outDocIds, AsyncCallback<Void> callback);

	void restore(List<Long> docIds, long folderId, AsyncCallback<Void> callback);

	void addBookmarks(List<Long> targetIds, int type, AsyncCallback<Void> callback);

	void deleteBookmarks(List<Long> bookmarkIds, AsyncCallback<Void> callback);

	void updateBookmark(GUIBookmark bookmark, AsyncCallback<Void> callback);

	void markHistoryAsRead(String event, AsyncCallback<Void> callback);

	void markIndexable(List<Long> docIds, int policy, AsyncCallback<Void> callback);

	void markUnindexable(List<Long> docIds, AsyncCallback<Void> callback);

	void cleanUploadedFileFolder(AsyncCallback<Void> callback);

	void getRating(long docId, AsyncCallback<GUIRating> callback);

	void saveRating(GUIRating rating, AsyncCallback<Integer> callback);

	void deleteNotes(List<Long> ids, AsyncCallback<Void> callback);

	void addNote(long docId, String message, AsyncCallback<Long> callback);

	void bulkUpdate(List<Long> ids, GUIDocument vo, boolean ignoreEmptyFields,
			AsyncCallback<List<GUIDocument>> callback);

	void addDocuments(boolean importZip, String charset, boolean immediateIndexing, GUIDocument metadata,
			AsyncCallback<List<GUIDocument>> callback);

	void updateNote(long docId, long noteId, String fileVersion, String message, AsyncCallback<Void> callback);

	void deleteVersions(List<Long> ids, AsyncCallback<GUIDocument> callback);

	void createWithContent(GUIDocument vo, String content, boolean checkout, AsyncCallback<GUIDocument> callback);

	void indexDocuments(List<Long> docIds, AsyncCallback<Void> callback);

	void deleteFromTrash(List<Long> ids, AsyncCallback<Void> callback);

	void emptyTrash(AsyncCallback<Void> callback);

	void archiveDocuments(List<Long> docIds, String comment, AsyncCallback<Void> callback);

	void archiveFolder(long folderId, String comment, AsyncCallback<Long> callback);

	void countDocuments(List<Long> folderIds, int status, AsyncCallback<Long> callback);

	void unarchiveDocuments(List<Long> docIds, AsyncCallback<Void> callback);

	void createTicket(long docId, int type, String suffix, Integer expireHours, Date expireDate, Integer maxDownloads,
			Integer maxViews, String password, AsyncCallback<List<String>> callback);

	void setTicketPassword(long ticketId, String password, AsyncCallback<List<String>> callback);

	void setPassword(long docId, String password, AsyncCallback<Void> callback);

	void unsetPassword(long docId, String currentPassword, AsyncCallback<Void> callback);

	void unprotect(long docId, String password, AsyncCallback<Boolean> callback);

	void getContentAsString(long docId, AsyncCallback<String> callback);

	void checkinContent(long docId, String content, AsyncCallback<GUIDocument> callback);

	void createDocument(GUIDocument document, String content, AsyncCallback<GUIDocument> callback);

	void getUserRating(long docId, AsyncCallback<GUIRating> callback);

	void deleteRating(long id, AsyncCallback<Integer> callback);

	void convert(long docId, String fileVersion, String format, AsyncCallback<GUIDocument> callback);

	void extractEmail(long docId, String fileVersion, AsyncCallback<GUIEmail> callback);

	void saveEmailAttachment(long docId, String fileVersion, String attachmentFileName,
			AsyncCallback<GUIDocument> callback);

	void replaceAlias(long aliasId, AsyncCallback<GUIDocument> callback);

	void deDuplicate(Long folderId, boolean retainNewest, AsyncCallback<Void> callback);

	void replaceFile(long docId, String fileVersion, String comment, AsyncCallback<Void> callback);

	void promoteVersion(long docId, String version, AsyncCallback<GUIDocument> callback);

	void getNotes(long docId, String fileVersion, Collection<String> types,
			AsyncCallback<List<GUIDocumentNote>> callback);

	void saveNotes(long docId, String fileVersion, List<GUIDocumentNote> notes, Collection<String> types,
			AsyncCallback<Void> callback);

	void deleteTicket(long ticketId, AsyncCallback<Void> callback);

	void enableTicket(long ticketId, AsyncCallback<Void> callback);

	void disableTicket(long ticketId, AsyncCallback<Void> callback);

	void enforceFilesIntoFolderStore(long folderId, AsyncCallback<Void> callback);

	void merge(List<Long> docIds, long targetFolderId, String fileName, AsyncCallback<GUIDocument> callback);

	void updatePages(long docId, AsyncCallback<Integer> callback);

	void rename(long documentId, String name, AsyncCallback<GUIDocument> callback);

	void validate(GUIDocument document, AsyncCallback<Void> callback);

	void getAllowedPermissions(List<Long> docIds, AsyncCallback<GUIAccessControlEntry> callback);

	void saveACL(GUIDocument document, AsyncCallback<Void> callback);

	void applyParentFolderSecurity(long docId, AsyncCallback<Void> callback);
}