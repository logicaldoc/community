package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;

/**
 * The client side stub for the Document Service. This service allows r/w
 * operations on documents.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
@RemoteServiceRelativePath("document")
public interface DocumentService extends RemoteService {
	/**
	 * Retrieves a specific document by its ID
	 */
	public GUIDocument getById(long docId) throws ServerException;

	/**
	 * Saves the document in the DB
	 * 
	 * @param document The document to save
	 * @return The saved document
	 */
	public GUIDocument save(GUIDocument document) throws Exception;

	/**
	 * Retrieves all attributes of the specified template
	 */
	public GUIAttribute[] getAttributes(long templateId) throws ServerException;

	/**
	 * Retrieves two specific versions by its ID
	 */
	public GUIVersion[] getVersionsById(long id1, long id2) throws ServerException;

	/**
	 * Sends a document as email(attachment or download ticket)
	 * 
	 * @return "ok" otherwise an error code
	 */
	public String sendAsEmail(GUIEmail email, String locale) throws ServerException;

	/**
	 * Extracts the email representation from a .eml or .msg file
	 * 
	 * @param docId The ID of the mail document
	 * @param fileVersion The File Version
	 * @return the Email representation
	 * @throws ServerException
	 */
	public GUIEmail extractEmail(long docId, String fileVersion) throws ServerException;

	/**
	 * Extracts an email attachment and saves it in the same folder of the
	 * document
	 * 
	 * @param docId The ID of the mail document
	 * @param fileVersion The File Version
	 * @return the newly created document
	 * @throws ServerException
	 */
	public GUIDocument saveEmailAttachment(long docId, String fileVersion, String attachmentFileName)
			throws ServerException;

	/**
	 * Updates the links type
	 * 
	 * @param id The link identifier
	 * @param type The new type to be set
	 */
	public void updateLink(long id, String type) throws ServerException;

	/**
	 * Deletes a selection of links
	 */
	public void deleteLinks(long[] ids) throws ServerException;

	/**
	 * Deletes a selection of versions
	 */
	public GUIDocument deleteVersions(long[] ids) throws ServerException;

	/**
	 * Links a set of documents
	 */
	public void linkDocuments(long[] inDocIds, long[] outDocIds) throws ServerException;

	/**
	 * Deletes a selection of documents
	 */
	public void delete(long[] ids) throws ServerException;

	/**
	 * Deletes a selection of documents from trash
	 */
	public void deleteFromTrash(Long[] ids) throws ServerException;

	/**
	 * Clear the user's trash
	 */
	public void emptyTrash() throws ServerException;

	/**
	 * Makes immutable a set of documents
	 */
	public void makeImmutable(long[] docIds, String comment) throws ServerException;

	/**
	 * Archives a set of documents
	 */
	public void archiveDocuments(long[] docIds, String comment) throws ServerException;

	/**
	 * Archives the documents in a folder
	 */
	public long archiveFolder(long folderId, String comment) throws ServerException;

	/**
	 * Converts a document in a given format
	 */
	public GUIDocument convert(long docId, String fileVersion, String format) throws ServerException;

	/**
	 * Counts the documents in a given status contained the specified folder's
	 * trees
	 */
	public long countDocuments(long[] folderIds, int status) throws ServerException;

	/**
	 * Unlocks a set of documents
	 */
	public void unlock(long[] docIds) throws ServerException;

	/**
	 * Locks a set of documents
	 */
	public void lock(long[] docIds, String comment) throws ServerException;

	/**
	 * Checks out the document
	 */
	public void checkout(long docId) throws ServerException;

	/**
	 * Adds new documents previously uploaded
	 * 
	 * @param language The language applied to all documents
	 * @param folderId The destination folder identifier
	 * @param importZip If .zip files have to be unpacked and the contained
	 *        documents imported
	 * @param charset Charset to use to process the .zip files
	 * @param immediteIndexing If the documents must be immediately indexed
	 * @param templateId The documents template
	 * 
	 * @return The list of created documents
	 */
	public GUIDocument[] addDocuments(String language, long folderId, boolean importZip, String charset,
			boolean immediateIndexing, Long templateId) throws ServerException;

	public GUIDocument[] addDocuments(boolean importZip, String charset, boolean immediateIndexing, GUIDocument metadata)
			throws ServerException;

	/**
	 * Create a download link for a given document
	 * 
	 * @param docId The document's ID
	 * @param suffix The suffix to download(optional)
	 * @param expireHours Number of hours after which the link expires
	 *        (optional)
	 * @param expireDate Exact expiration date (optional)
	 * @return The created download link
	 * @throws ServerException
	 */
	public String createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate)
			throws ServerException;

	/**
	 * Indexes the given set of documents
	 * 
	 * @param docIds The set of documents to index
	 */
	public void indexDocuments(Long[] docIds) throws ServerException;

	/**
	 * Checks-in a new document version
	 * 
	 * @param document The document to update
	 * @param major True if this is a major version
	 * 
	 * @returns The updated document
	 */
	public GUIDocument checkin(GUIDocument document, boolean major) throws Exception;

	/**
	 * Checks-in a new document's text content
	 */
	public GUIDocument checkinContent(long docId, String content) throws Exception;

	/**
	 * Creates a new document with the given text content.
	 */
	public GUIDocument createDocument(GUIDocument document, String content) throws Exception;

	/**
	 * Retrieves the document's content as a string
	 */
	public String getContentAsString(long docId) throws ServerException;

	/**
	 * Restores a given document
	 */
	public void restore(Long[] docIds, long folderId) throws ServerException;

	/**
	 * Restores a given set of archived documents
	 */
	public void unarchiveDocuments(long[] docIds) throws ServerException;

	/**
	 * Adds new bookmarks
	 */
	public void addBookmarks(long[] targetIds, int type) throws ServerException;

	/**
	 * Deletes a set of bookmarks
	 */
	public void deleteBookmarks(long[] bookmarkIds) throws ServerException;

	/**
	 * Updates a single bookmark's data
	 */
	public void updateBookmark(GUIBookmark bookmark) throws ServerException;

	/**
	 * Marks as read the histories related to the current user and the given
	 * event.
	 * 
	 * @param event The history event to mark as read
	 */
	public void markHistoryAsRead(String event) throws ServerException;

	/**
	 * Marks a set of documents as unindexable
	 */
	public void markUnindexable(long[] docIds) throws ServerException;

	/**
	 * Marks a set of documents as indexable
	 */
	public void markIndexable(long[] docIds) throws ServerException;

	/**
	 * Cleans the uploaded files folder.
	 */
	public void cleanUploadedFileFolder() throws ServerException;

	/**
	 * Retrieves the rating of the given document.
	 */
	public GUIRating getRating(long docId) throws ServerException;

	/**
	 * Save a rating vote on a document.
	 * 
	 * @return the new document rating value
	 */
	public int saveRating(GUIRating rating) throws ServerException;

	/**
	 * Adds a new document note on the given document
	 */
	public long addNote(long docId, String message) throws ServerException;

	/**
	 * Updates a document note on the given document
	 */
	public void updateNote(long docId, long noteId, String message) throws ServerException;

	/**
	 * Deletes a selection of document notes
	 */
	public void deleteNotes(long[] ids) throws ServerException;

	/**
	 * Applies to a selection of documents all the given data.
	 */
	public void bulkUpdate(long[] ids, GUIDocument vo, boolean ignoreEmptyFields) throws ServerException;

	/**
	 * Creates a new empty document
	 */
	public GUIDocument createEmpty(GUIDocument vo) throws ServerException;

	/**
	 * Puts a password protection to the document
	 */
	public void setPassword(long docid, String password) throws ServerException;

	/**
	 * Removes the password protection from the document
	 */
	public void unsetPassword(long docId, String currentPassword) throws ServerException;

	/**
	 * Checks if the document can be accessed with the given password
	 */
	public boolean unprotect(long docId, String password) throws ServerException;

	/**
	 * Retrieves the vote of the current user on the specified document
	 */
	public GUIRating getUserRating(long docId) throws ServerException;

	/**
	 * Deletes a vote
	 */
	public Integer deleteRating(long id) throws ServerException;

	/**
	 * Replaces an alias with a copy of the original file
	 * 
	 * @param aliasId ID of the alias to replace
	 */
	public GUIDocument replaceAlias(long aliasId) throws ServerException;

	/**
	 * Convert duplicates with aliases, just one of the documents is maintained
	 * 
	 * @param folderId optional ID of the folder to process
	 * @param retainNewest true if the newest has to be retained, otherwise it will be the oldest.
	 */
	public void deDuplicate(Long folderId, boolean retainNewest) throws ServerException;
	
	public static class Instance {
		private static DocumentServiceAsync instance;

		public static DocumentServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(DocumentService.class);
			}
			return instance;
		}
	}
}