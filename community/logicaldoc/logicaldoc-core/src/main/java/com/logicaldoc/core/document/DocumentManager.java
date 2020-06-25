package com.logicaldoc.core.document;

import java.io.File;
import java.io.InputStream;
import java.util.Date;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;

/**
 * A general manager for documents handling issues
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public interface DocumentManager {

	/**
	 * Checks in the given document
	 * 
	 * @param docId the document to be checked in
	 * @param fileInputStream input stream pointing to the new document version
	 * @param filename new filename (can also be the old one)
	 * @param release True if this is a new release(eg: 2.0) rather than a
	 *        subversion(eg: 1.1)
	 * @param docVO The value object containing document's metadata applied
	 *        during the checkin (optional)
	 * @param transaction entry to log the event, set the user and comment
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void checkin(long docId, InputStream fileInputStream, String filename, boolean release,
			AbstractDocument docVO, DocumentHistory transaction) throws Exception;

	/**
	 * Checks in the given document
	 * 
	 * @param docId the document to be checked in
	 * @param file of the new document version
	 * @param filename new filename (can also be the old one)
	 * @param release True if this is a new release(eg: 2.0) rather than a
	 *        subversion(eg: 1.1)
	 * @param docVO The value object containing document's metadata applied
	 *        during the checkin (optional)
	 * @param transaction entry to log the event, set the user and comment
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void checkin(long docId, File file, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws Exception;

	/**
	 * Checks out the given document
	 * 
	 * @param docId the document to be checked out
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void checkout(long docId, DocumentHistory transaction) throws Exception;

	/**
	 * Replaces the file of a given version
	 * 
	 * @param docId the document to be updated
	 * @param fileVersion the file version
	 * @param newFile the file to use
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void replaceFile(long docId, String fileVersion, File newFile, DocumentHistory transaction) throws Exception;

	/**
	 * Replaces the file of a given version
	 * 
	 * @param docId the document to be updated
	 * @param fileVersion the file version
	 * @param newFile the file to use
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void replaceFile(long docId, String fileVersion, InputStream newFile, DocumentHistory transaction)
			throws Exception;

	/**
	 * Promotes an old version to the current default one. If you promote a
	 * prior version, what it does is make it the default version again.
	 * (regardless of there being many versions).
	 * 
	 * @param docId the document to be updated
	 * @param version the version
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void promoteVersion(long docId, String version, DocumentHistory transaction) throws Exception;

	/**
	 * Locks the given document
	 * 
	 * @param docId the document to be locked
	 * @param status the lock type (used to populate status attribute of the
	 *        document)
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void lock(long docId, int status, DocumentHistory transaction) throws Exception;

	/**
	 * UNChecks out the given document
	 * 
	 * @param docId the document to be unchecked out
	 * @param transaction entry to log the event
	 * 
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void unlock(long docId, DocumentHistory transaction) throws Exception;

	/**
	 * Creates a new Document. Saves the information provided. That also
	 * includes updating the search index for example.
	 * 
	 * @param file The document's file
	 * @param docVO The value object containing the document's metadata
	 * @param transaction The trandaction metadata (remember to set the user and
	 *        the comment)
	 * @return The newly created document
	 * 
	 * @throws Exception raised if the document cannot be created
	 */
	public Document create(File file, Document docVO, DocumentHistory transaction) throws Exception;

	/**
	 * Creates a new Document. Saves the information provided. That also
	 * includes updating the search index for example.
	 * 
	 * @param content The document's content
	 * @param docVO The value object containing the document's metadata
	 * @param transaction The transaction metadata (remember to set the user and
	 *        the comment)
	 * @return The newly created document
	 * 
	 * @throws Exception raised if the document cannot be created
	 */
	public Document create(InputStream content, Document docVO, DocumentHistory transaction) throws Exception;

	/**
	 * Re-indexes an existing document in the full-text index.
	 * 
	 * @param docId The document to be indexed
	 * @param content The content to use as document's body (can be null to
	 *        parse the file)
	 * @return the number of milliseconds required to parse the document
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public long reindex(long docId, String content) throws Exception;

	/**
	 * Rename an existing document filename.
	 * 
	 * @param docId The document to be renamed
	 * @param newName The new filename of the document
	 * @param transaction entry to log the event (set the user)
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void rename(long docId, String newName, DocumentHistory transaction) throws Exception;

	/**
	 * Updates an existing document and marks it to be re-indexed
	 * 
	 * @param doc The document to be updated
	 * @param docVO value object containing the new metadata
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws Exception if an error occurs, this exception is thrown
	 */
	public void update(Document doc, Document docVO, DocumentHistory transaction) throws Exception;

	/**
	 * Utility method for document removal from index and database update(flag
	 * indexed)
	 * 
	 * @param doc the document to remove from the index
	 */
	public void deleteFromIndex(Document doc);

	/**
	 * Utility method used to declare that:
	 * <ol>
	 * <li>the document must be taken into consideration by the indexer (status
	 * = {@link AbstractDocument#INDEX_TO_INDEX}.</li>
	 * <li>the document must be taken into consideration by the indexer for the
	 * metadata only(status =
	 * {@link AbstractDocument#INDEX_TO_INDEX_METADATA}.</li>
	 * <li>the document must not be taken into consideration by the indexer
	 * (status = {@link AbstractDocument#INDEX_SKIP}). If the document was
	 * previously indexed it is removed from the index.</li>
	 * </ol>
	 * 
	 * Status:
	 * <ol>
	 * <li>{@link AbstractDocument#INDEX_TO_INDEX}</li>
	 * <li>{@link AbstractDocument#INDEX_TO_INDEX_METADATA}</li>
	 * <li>{@link AbstractDocument#INDEX_SKIP}</li>
	 * </ol>
	 * 
	 * @param doc The document for which will be changed the indexer status.
	 * @param status The new document indexer status.
	 */
	public void changeIndexingStatus(Document doc, int status);

	/**
	 * Marks the document, with the given docId, as immutable and save the given
	 * document history
	 * 
	 * @param docId identifier of the document
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws Exception raised if the document cannot be marked immutable
	 */
	public void makeImmutable(long docId, DocumentHistory transaction) throws Exception;

	/**
	 * Moves a document to the specified folder. All stores(db, file system,
	 * index) will be consequently altered.
	 * 
	 * @param doc The document to move
	 * @param folder The target folder
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws Exception raised if the document cannot be moved
	 */
	public void moveToFolder(Document doc, Folder folder, DocumentHistory transaction) throws Exception;

	/**
	 * Copy a document to the specified folder.
	 * 
	 * @param doc The document to move
	 * @param folder The target folder
	 * @param transaction entry to log the event (set the user)
	 * @return The created document
	 * 
	 * @throws Exception raised if the document cannot be copied
	 */
	public Document copyToFolder(Document doc, Folder folder, DocumentHistory transaction) throws Exception;

	/**
	 * Create an alias(shortcut) associated to the given doc to the specified
	 * folder.
	 * 
	 * @param doc The document for which will be created the shortcut
	 * @param folder The target folder
	 * @param type the alias type(<b>null</b> for the original file or
	 *        <b>pdf</b> for it's pdf conversion)
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return The created document
	 * 
	 * @throws Exception raised if the alias cannot be created
	 */
	public Document createAlias(Document doc, Folder folder, String type, DocumentHistory transaction) throws Exception;

	/**
	 * Replaces an alias with a copy of the original file
	 * 
	 * @param aliasId ID of the alias to replace
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return The created document
	 * 
	 * @throws Exception raised if the alias cannot be replaced
	 */
	public Document replaceAlias(long aliasId, DocumentHistory transaction) throws Exception;

	/**
	 * Deletes a specific version.
	 * 
	 * @param versionId The version to delete
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return the latest version
	 * 
	 * @throws Exception If the version cannot be deleted
	 */
	public Version deleteVersion(long versionId, DocumentHistory transaction) throws Exception;

	/**
	 * Retrieves the document's content as a string
	 * 
	 * @param doc the document representation
	 * @param fileVersion version fo the file
	 * 
	 * @return The document's content
	 */
	public String parseDocument(Document doc, String fileVersion);

	/**
	 * Archives all the documents in a folder's tree
	 * 
	 * @param folderId The root folder
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return Total number of archived documents
	 * 
	 * @throws Exception raised if at least a document cannot be archived
	 */
	public long archiveFolder(long folderId, DocumentHistory transaction) throws Exception;

	/**
	 * Archives all the documents in a folder's tree
	 * 
	 * @param docIds Documents to be archived
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws Exception raised if at least a document cannot be archived
	 */
	public void archiveDocuments(long[] docIds, DocumentHistory transaction) throws Exception;

	public void setStorer(Storer storer);

	/**
	 * Creates a new download ticket.
	 * 
	 * @param docId The document id
	 * @param suffix can be null or 'conversion.pdf'
	 * @param expireHours expiration time expressed in hours
	 * @param expireDate exact expiration date expressed in the format
	 *        yyyy-MM-dd
	 * @param maxDownloads maximum number of admitted downloads
	 * @param urlPrefix prefiz of the url, by default the general setting
	 *        'server.url' will be used
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return The created ticket with the url property filled
	 * 
	 * @throws Exception raised if the download ticket cannot be created
	 */
	public Ticket createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads, String urlPrefix, DocumentHistory transaction) throws Exception;

	/**
	 * Tries to unprotect a document, If the password is correct, the document
	 * stays unprotected for all the duration of the session.
	 * 
	 * @param sid Session identifier
	 * @param docId The document id
	 * @param password The password to try
	 * 
	 * @return True if the file gets unprotected
	 */
	public boolean unprotect(String sid, long docId, String password);
}