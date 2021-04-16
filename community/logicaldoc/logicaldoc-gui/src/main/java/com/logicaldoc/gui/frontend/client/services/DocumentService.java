package com.logicaldoc.gui.frontend.client.services;

import java.util.Collection;
import java.util.Date;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIRating;
import com.logicaldoc.gui.common.client.beans.GUIVersion;

/**
 * The client side stub for the Document Service. This service allows r/w
 * operations on documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("document")
public interface DocumentService extends RemoteService {
	/**
	 * Retrieves a specific document by its ID
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the document retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument getById(long docId) throws ServerException;

	/**
	 * Saves the document in the DB
	 * 
	 * @param document The document to save
	 * 
	 * @return The saved document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument save(GUIDocument document) throws Exception;

	/**
	 * Retrieves all attributes of the specified template
	 * 
	 * @param templateId identifier of the template
	 * 
	 * @return the attributes
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAttribute[] getAttributes(long templateId) throws ServerException;

	/**
	 * Retrieves two specific versions by its ID
	 * 
	 * @param id1 identifier of the first version
	 * @param id2 identifier of the second version
	 * 
	 * @return the two versions
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIVersion[] getVersionsById(long id1, long id2) throws ServerException;

	/**
	 * Sends a document as email(attachment or download ticket)
	 * 
	 * @param email the email to send
	 * @param locale the locale specification
	 * 
	 * @return "ok" otherwise an error code
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String sendAsEmail(GUIEmail email, String locale) throws ServerException;

	/**
	 * Extracts the email representation from a .eml or .msg file
	 * 
	 * @param docId the identifier of the mail document
	 * @param fileVersion the File Version
	 * 
	 * @return the email representation
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIEmail extractEmail(long docId, String fileVersion) throws ServerException;

	/**
	 * Extracts an email attachment and saves it in the same folder of the
	 * document
	 * 
	 * @param docId the identifier of the mail document
	 * @param fileVersion the File Version
	 * @param attachmentFileName name of the attachment
	 * 
	 * @return the just created document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument saveEmailAttachment(long docId, String fileVersion, String attachmentFileName)
			throws ServerException;

	/**
	 * Updates the links type
	 * 
	 * @param id The link identifier
	 * @param type The new type to be set
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void updateLink(long id, String type) throws ServerException;

	/**
	 * Deletes a selection of links
	 * 
	 * @param ids identifiers of the links
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteLinks(long[] ids) throws ServerException;

	/**
	 * Deletes a selection of versions
	 * 
	 * @param ids identifiers of the versions
	 * 
	 * @return the document the deleted versions belongs to
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument deleteVersions(long[] ids) throws ServerException;

	/**
	 * Links a set of documents
	 * 
	 * @param inDocIds identifiers of the documents for the IN direction
	 * @param outDocIds identifiers of the documents for the OUT direction
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void linkDocuments(long[] inDocIds, long[] outDocIds) throws ServerException;

	/**
	 * Deletes a selection of documents
	 * 
	 * @param ids identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long[] ids) throws ServerException;

	/**
	 * Deletes a selection of documents from trash
	 * 
	 * @param ids identifiers of documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteFromTrash(Long[] ids) throws ServerException;

	/**
	 * Clear the user's trash
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void emptyTrash() throws ServerException;

	/**
	 * Makes immutable a set of documents
	 * 
	 * @param docIds identifiers of the documents
	 * @param comment the commit
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void makeImmutable(long[] docIds, String comment) throws ServerException;

	/**
	 * Archives a set of documents
	 * 
	 * @param docIds identifiers of the documents
	 * @param comment the commit
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void archiveDocuments(long[] docIds, String comment) throws ServerException;

	/**
	 * Archives the documents in a folder
	 * 
	 * @param folderId identifier of the folder
	 * @param comment a comment for the action
	 * 
	 * @return number of records added to the archive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public long archiveFolder(long folderId, String comment) throws ServerException;

	/**
	 * Converts a document in a given format
	 * 
	 * @param docId identifier of the document to convert
	 * @param fileVersion version of the file
	 * @param format the format to convert to (e.g.: <b>pdf</b>, <b>txt</b>, ...
	 * 
	 * @return the converted document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument convert(long docId, String fileVersion, String format) throws ServerException;

	/**
	 * Counts the documents in a given status contained the specified folder's
	 * trees
	 * 
	 * @param folderIds identifiers of the folders
	 * @param status a filter on the document's status
	 * 
	 * @return the count
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public long countDocuments(long[] folderIds, int status) throws ServerException;

	/**
	 * Unlocks a set of documents
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void unlock(long[] docIds) throws ServerException;

	/**
	 * Locks a set of documents
	 * 
	 * @param docIds identifiers of the documents
	 * @param comment the comment to the lock
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void lock(long[] docIds, String comment) throws ServerException;

	/**
	 * Checks out the document
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void checkout(long[] docIds) throws ServerException;

	/**
	 * Adds new documents previously uploaded
	 * 
	 * @param language The language applied to all documents
	 * @param folderId The destination folder identifier
	 * @param importZip If .zip files have to be unpacked and the contained
	 *        documents imported
	 * @param charset Charset to use to process the .zip files
	 * @param immediateIndexing If the documents must be immediately indexed
	 * @param templateId The documents template
	 * 
	 * @return The list of created documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument[] addDocuments(String language, long folderId, boolean importZip, String charset,
			boolean immediateIndexing, Long templateId) throws ServerException;

	public GUIDocument[] addDocuments(boolean importZip, String charset, boolean immediateIndexing,
			GUIDocument metadata) throws ServerException;

	public String createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads) throws ServerException;

	public void deleteTicket(long ticketId) throws ServerException;

	public void enableTicket(long ticketId) throws ServerException;

	public void disableTicket(long ticketId) throws ServerException;

	/**
	 * Indexes the given set of documents
	 * 
	 * @param docIds The set of documents to index
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void indexDocuments(Long[] docIds) throws ServerException;

	/**
	 * Checks-in a new document version
	 * 
	 * @param document The document to update
	 * @param major True if this is a major version
	 * 
	 * @return The updated document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument checkin(GUIDocument document, boolean major) throws ServerException;

	/**
	 * Checks-in a new document's text content
	 * 
	 * @param docId identifier of the document
	 * @param content content of the file
	 * 
	 * @return the document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument checkinContent(long docId, String content) throws ServerException;

	/**
	 * Replaces the file associated to a given version
	 * 
	 * @param docId the identifier of the document
	 * @param fileVersion the file version
	 * @param comment the comment
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void replaceFile(long docId, String fileVersion, String comment) throws ServerException;

	/**
	 * If you promote a prior version, what it does is make it the default
	 * version again. (regardless of there being many versions)
	 * 
	 * @param docId the identifier of the document
	 * @param version version specification
	 * 
	 * @return the document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument promoteVersion(long docId, String version) throws ServerException;

	/**
	 * Creates a new document with the given text content
	 * 
	 * @param document the document to create
	 * @param content the contents
	 * 
	 * @return the created document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument createDocument(GUIDocument document, String content) throws ServerException;

	/**
	 * Retrieves the document's content as a string
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the document's extracted content
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String getContentAsString(long docId) throws ServerException;

	/**
	 * Restores a given document
	 * 
	 * @param docIds identifiers of the documents
	 * @param folderId identifier of the folder in which to restore the
	 *        documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void restore(Long[] docIds, long folderId) throws ServerException;

	/**
	 * Restores a given set of archived documents
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void unarchiveDocuments(long[] docIds) throws ServerException;

	/**
	 * Adds new bookmarks
	 * 
	 * @param targetIds identfiers of the documents or folders to bookmark
	 * @param type the type of bookmark (<b>0</b> = document, <b>1</b> = folder)
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void addBookmarks(long[] targetIds, int type) throws ServerException;

	/**
	 * Deletes a set of bookmarks
	 * 
	 * @param bookmarkIds identifiers of the bookmarks to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteBookmarks(long[] bookmarkIds) throws ServerException;

	/**
	 * Updates a single bookmark's data
	 * 
	 * @param bookmark the bookmark to update
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void updateBookmark(GUIBookmark bookmark) throws ServerException;

	/**
	 * Marks as read the histories related to the current user and the given
	 * event.
	 * 
	 * @param event The history event to mark as read
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void markHistoryAsRead(String event) throws ServerException;

	/**
	 * Marks a set of documents as unindexable
	 * 
	 * @param docIds identifiers of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void markUnindexable(long[] docIds) throws ServerException;

	/**
	 * Marks a set of documents as indexable
	 * 
	 * @param docIds identifiers of the documents
	 * @param policy indexing policy:
	 *        {@link com.logicaldoc.gui.common.client.Constants#INDEX_TO_INDEX}
	 *        or
	 *        {@link com.logicaldoc.gui.common.client.Constants#INDEX_TO_INDEX_METADATA}
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void markIndexable(long[] docIds, int policy) throws ServerException;

	/**
	 * Cleans the uploaded files folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void cleanUploadedFileFolder() throws ServerException;

	/**
	 * Retrieves the rating of the given document
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return the rating retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRating getRating(long docId) throws ServerException;

	/**
	 * Save a rating vote on a document
	 * 
	 * @param rating the document's rating
	 * 
	 * @return the new document rating value
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public int saveRating(GUIRating rating) throws ServerException;

	/**
	 * Adds a new document note on the given document
	 * 
	 * @param docId identifier of the document
	 * @param note the note's text
	 * 
	 * @return identifier of the created note
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public long addNote(long docId, String note) throws ServerException;

	/**
	 * Updates a document note on the given document
	 * 
	 * @param docId identifier of the document
	 * @param noteId identifier of the note
	 * @param note text of the note
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void updateNote(long docId, long noteId, String note) throws ServerException;

	/**
	 * Retrieves the notes of a document
	 * 
	 * @param docId identifier of the document
	 * @param fileVersion file version specification
	 * @param types optional filter for the note type
	 * 
	 * @return the notes on the given version
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocumentNote[] getNotes(long docId, String fileVersion, Collection<String> types) throws ServerException;

	/**
	 * Saves a set of notes
	 * 
	 * @param docId identifier of the document
	 * @param notes the notes to save
	 * @param types optional filter for the note type
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveNotes(long docId, GUIDocumentNote[] notes, Collection<String> types) throws ServerException;

	/**
	 * Deletes a selection of document notes
	 * 
	 * @param ids identifiers of the notes
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteNotes(long[] ids) throws ServerException;

	/**
	 * Applies to a selection of documents all the given data
	 * 
	 * @param ids identifiers of the documents to update
	 * @param vo the value object to use as template
	 * @param ignoreEmptyFields flag to skip fields empty in the <code>vo</code>
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void bulkUpdate(long[] ids, GUIDocument vo, boolean ignoreEmptyFields) throws ServerException;

	/**
	 * Creates a new empty document
	 * 
	 * @param vo the value object to use as template
	 * @param content the text body of the new document
	 * 
	 * @return the created document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument createWithContent(GUIDocument vo, String content) throws ServerException;

	/**
	 * Puts a password protection to the document
	 * 
	 * @param docId the identifier of the document to protect
	 * @param password the password to assign
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setPassword(long docId, String password) throws ServerException;

	/**
	 * Removes the password protection from the document
	 * 
	 * @param docId the identifier of the document to unprotect
	 * @param password the password to clear
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void unsetPassword(long docId, String password) throws ServerException;

	/**
	 * Checks if the document can be accessed with the given password
	 * 
	 * @param docId the identifier of the document to unprotect
	 * @param password the password to clear
	 * 
	 * @return is the password is correct
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean unprotect(long docId, String password) throws ServerException;

	/**
	 * Retrieves the vote of the current user on the specified document
	 * 
	 * @param docId the identifier of the document
	 * 
	 * @return the rating
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIRating getUserRating(long docId) throws ServerException;

	/**
	 * Deletes a vote
	 * 
	 * @param id identifier of the rating to delete
	 * 
	 * @return the result
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public Integer deleteRating(long id) throws ServerException;

	/**
	 * Replaces an alias with a copy of the original file
	 * 
	 * @param aliasId ID of the alias to replace
	 * 
	 * @return the just created document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument replaceAlias(long aliasId) throws ServerException;

	/**
	 * Convert duplicates with aliases, just one of the documents is maintained
	 * 
	 * @param folderId optional ID of the folder to process
	 * @param retainNewest true if the newest has to be retained, otherwise it
	 *        will be the oldest.
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deDuplicate(Long folderId, boolean retainNewest) throws ServerException;

	/**
	 * Enforces that all the files in the given tree are stored in the storage
	 * configured in the owning folder. The process is asynchronous, at the end
	 * an internal message to the user will be sent to alert him about its end.
	 * 
	 * @param folderId identifier of the tree root
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void enforceFilesIntoFolderStorage(long folderId) throws ServerException;

	/**
	 * Merges a set of documents into a single PDF
	 * 
	 * @param docIds identifiers of the documents to merge
	 * 
	 * @param targetFolderId identifier of the folder that will receive the
	 *        merged PDF
	 * @param fileName file name of the merged file
	 * 
	 * @return the newly created merged document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument merge(long[] docIds, long targetFolderId, String fileName) throws ServerException;
	
	public static class Instance {
		private static DocumentServiceAsync instance;

		public static DocumentServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(DocumentService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}