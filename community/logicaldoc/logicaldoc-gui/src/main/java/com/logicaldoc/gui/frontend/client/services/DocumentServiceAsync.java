package com.logicaldoc.gui.frontend.client.services;

import java.util.Collection;
import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;

public interface DocumentServiceAsync {
	
	void getVersionsById(long id1, long id2, AsyncCallback<GUIVersion[]> callback);

	void getAttributes(long templateId, AsyncCallback<GUIAttribute[]> callback);

	void getById(long docId, AsyncCallback<GUIDocument> callback);

	void save(GUIDocument document, AsyncCallback<GUIDocument> callback);

	void sendAsEmail(GUIEmail email, String locale, AsyncCallback<String> callback);

	void updateLink(long id, String type, AsyncCallback<Void> callback);

	void deleteLinks(long[] ids, AsyncCallback<Void> callback);

	void delete(long[] ids, AsyncCallback<Void> callback);

	void makeImmutable(long[] ids, String comment, AsyncCallback<Void> callback);

	void lock(long[] ids, String comment, AsyncCallback<Void> callback);

	void unlock(long[] ids, AsyncCallback<Void> callback);

	void addDocuments(String language, long folderId, boolean importZip, String charset, boolean immediateIndexing,
			Long templateId, AsyncCallback<GUIDocument[]> callback);

	void checkout(long[] docIds, AsyncCallback<Void> callback);

	void checkin(GUIDocument document, boolean major, AsyncCallback<GUIDocument> callback);

	void linkDocuments(long[] inDocIds, long[] outDocIds, AsyncCallback<Void> callback);

	void restore(Long[] docIds, long folderId, AsyncCallback<Void> callback);

	void addBookmarks(long[] targetIds, int type, AsyncCallback<Void> callback);

	void deleteBookmarks(long[] bookmarkIds, AsyncCallback<Void> callback);

	void updateBookmark(GUIBookmark bookmark, AsyncCallback<Void> callback);

	void markHistoryAsRead(String event, AsyncCallback<Void> callback);

	void markIndexable(long[] docIds, int policy, AsyncCallback<Void> callback);

	void markUnindexable(long[] docIds, AsyncCallback<Void> callback);

	void cleanUploadedFileFolder(AsyncCallback<Void> callback);

	void getRating(long docId, AsyncCallback<GUIRating> callback);

	void saveRating(GUIRating rating, AsyncCallback<Integer> callback);

	void deleteNotes(long[] ids, AsyncCallback<Void> callback);

	void addNote(long docId, String message, AsyncCallback<Long> callback);

	void bulkUpdate(long[] ids, GUIDocument vo, boolean ignoreEmptyFields, AsyncCallback<Void> callback);

	void addDocuments(boolean importZip, String charset, boolean immediateIndexing, GUIDocument metadata,
			AsyncCallback<GUIDocument[]> callback);

	void updateNote(long docId, long noteId, String message, AsyncCallback<Void> callback);

	void deleteVersions(long[] ids, AsyncCallback<GUIDocument> callback);

	void createWithContent(GUIDocument vo, String content, AsyncCallback<GUIDocument> callback);

	void indexDocuments(Long[] docIds, AsyncCallback<Void> callback);

	void deleteFromTrash(Long[] ids, AsyncCallback<Void> callback);

	void emptyTrash(AsyncCallback<Void> callback);

	void archiveDocuments(long[] docIds, String comment, AsyncCallback<Void> callback);

	void archiveFolder(long folderId, String comment, AsyncCallback<Long> callback);

	void countDocuments(long[] folderIds, int status, AsyncCallback<Long> callback);

	void unarchiveDocuments(long[] docIds, AsyncCallback<Void> callback);

	void createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate, Integer maxDownloads,
			AsyncCallback<String> callback);

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

	void getNotes(long docId, String fileVersion, Collection<String> types, AsyncCallback<GUIDocumentNote[]> callback);

	void saveNotes(long docId, GUIDocumentNote[] notes, Collection<String> types, AsyncCallback<Void> callback);

	void deleteTicket(long ticketId, AsyncCallback<Void> callback);

	void enableTicket(long ticketId, AsyncCallback<Void> callback);

	void disableTicket(long ticketId, AsyncCallback<Void> callback);

	void enforceFilesIntoFolderStorage(long folderId, AsyncCallback<Void> callback);

	void merge(long[] docIds, long targetFolderId, String fileName, AsyncCallback<GUIDocument> callback);
}