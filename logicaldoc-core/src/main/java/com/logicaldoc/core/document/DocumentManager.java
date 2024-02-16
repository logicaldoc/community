package com.logicaldoc.core.document;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.parser.ParseException;
import com.logicaldoc.core.security.authorization.PermissionException;
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
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException error at data layer
	 */
	public void checkin(long docId, InputStream fileInputStream, String filename, boolean release,
			AbstractDocument docVO, DocumentHistory transaction) throws IOException, PersistenceException;

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
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void checkin(long docId, File file, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws PersistenceException;

	/**
	 * Checks out the given document
	 * 
	 * @param docId the document to be checked out
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void checkout(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Replaces the file of a given version
	 * 
	 * @param docId the document to be updated
	 * @param fileVersion the file version
	 * @param newFile the file to use
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException error at data layer
	 */
	public void replaceFile(long docId, String fileVersion, File newFile, DocumentHistory transaction)
			throws PersistenceException, IOException;

	/**
	 * Replaces the file of a given version
	 * 
	 * @param docId the document to be updated
	 * @param fileVersion the file version
	 * @param newFile the file to use
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws IOException I/O error
	 */
	public void replaceFile(long docId, String fileVersion, InputStream newFile, DocumentHistory transaction)
			throws IOException, PersistenceException;

	/**
	 * Promotes an old version to the current default one. If you promote a
	 * prior version, what it does is make it the default version again.
	 * (regardless of there being many versions).
	 * 
	 * @param docId the document to be updated
	 * @param version the version
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws IOException I/O error
	 */
	public void promoteVersion(long docId, String version, DocumentHistory transaction)
			throws PersistenceException, IOException;

	/**
	 * Locks the given document
	 * 
	 * @param docId the document to be locked
	 * @param status the lock type (used to populate status attribute of the
	 *        document)
	 * @param transaction entry to log the event (set the user)
	 * @throws PersistenceException if an error occurs, this exception is thrown
	 */
	public void lock(long docId, int status, DocumentHistory transaction) throws PersistenceException;

	/**
	 * UNChecks out the given document
	 * 
	 * @param docId the document to be unchecked out
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException if an error occurs, this exception is thrown
	 */
	public void unlock(long docId, DocumentHistory transaction) throws PersistenceException;

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
	 * @throws PersistenceException raised if the document cannot be created
	 */
	public Document create(File file, Document docVO, DocumentHistory transaction) throws PersistenceException;

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
	 * @throws PersistenceException raised if the document cannot be created
	 */
	public Document create(InputStream content, Document docVO, DocumentHistory transaction)
			throws PersistenceException;

	/**
	 * Re-indexes an existing document in the full-text index.
	 * 
	 * @param docId The document to be indexed
	 * @param content The content to use as document's body (can be null to
	 *        parse the file)
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return the number of milliseconds required to parse the document
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws ParseException error during parsing
	 */
	public long index(long docId, String content, DocumentHistory transaction)
			throws PersistenceException, ParseException;

	/**
	 * Rename an existing document filename.
	 * 
	 * @param docId The document to be renamed
	 * @param newName The new filename of the document
	 * @param transaction entry to log the event (set the user)
	 * @throws PersistenceException if an error occurs, this exception is thrown
	 */
	public void rename(long docId, String newName, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Updates an existing document and marks it to be re-indexed
	 * 
	 * @param doc The document to be updated
	 * @param docVO value object containing the new metadata
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException if an error occurs, this exception is thrown
	 */
	public void update(Document doc, Document docVO, DocumentHistory transaction) throws PersistenceException;

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
	 * @throws PersistenceException raised if the document cannot be marked
	 *         immutable
	 */
	public void makeImmutable(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Moves a document to the specified folder. All stores(db, file system,
	 * index) will be consequently altered.
	 * 
	 * @param doc The document to move
	 * @param folder The target folder
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException raised if the document cannot be moved
	 */
	public void moveToFolder(Document doc, Folder folder, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Copy a document to the specified folder.
	 * 
	 * @param doc The document to move
	 * @param folder The target folder
	 * @param transaction entry to log the event (set the user)
	 * @param links if links must be copied too
	 * @param notes if notes and annotations must be copied too
	 * @param notes if security settings must be copied too
	 * 
	 * @return The created document
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws IOException I/O error
	 */
	public Document copyToFolder(Document doc, Folder folder, DocumentHistory transaction, boolean links, boolean notes,
			boolean security) throws PersistenceException, IOException;

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
	 * @throws PersistenceException error at data layer
	 */
	public Document createAlias(Document doc, Folder folder, String type, DocumentHistory transaction)
			throws PersistenceException;

	/**
	 * Replaces an alias with a copy of the original file
	 * 
	 * @param aliasId ID of the alias to replace
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return The created document
	 * 
	 * @throws PersistenceException raised if the alias cannot be replaced
	 */
	public Document replaceAlias(long aliasId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Deletes a specific version.
	 * 
	 * @param versionId The version to delete
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return the latest version
	 * 
	 * @throws PersistenceException If the version cannot be deleted
	 */
	public Version deleteVersion(long versionId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Retrieves the document's content as a string
	 * 
	 * @param doc the document representation
	 * @param fileVersion version of the file
	 * 
	 * @return The document's content
	 * 
	 * @throws ParseException error in the parsing
	 */
	public String parseDocument(Document doc, String fileVersion) throws ParseException;

	/**
	 * Archives all the documents in a folder's tree
	 * 
	 * @param folderId The root folder
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return Total number of archived documents
	 * 
	 * @throws PersistenceException raised if at least a document cannot be
	 *         archived
	 */
	public long archiveFolder(long folderId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Archives all the documents in a folder's tree
	 * 
	 * @param docIds Documents to be archived
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @throws PersistenceException raised if at least a document cannot be
	 *         archived
	 */
	public void archiveDocuments(Set<Long> docIds, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Creates a new ticket.
	 * 
	 * @param ticket Value object carrying the metadata for the ticket to create
	 * @param transaction entry to log the event (set the user)
	 * 
	 * @return The created ticket with the url property filled
	 * 
	 * @throws PersistenceException raised if the download ticket cannot be
	 *         created
	 * @throws PermissionException raised if the user does not have the download
	 *         permission
	 */
	public Ticket createTicket(Ticket ticket, DocumentHistory transaction)
			throws PersistenceException, PermissionException;

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

	/**
	 * Moves all the files of the documents in the given tree from it's original
	 * location to the storage defined in the owning folder
	 * 
	 * @param rootFolderId identifier of the root of the tree to process
	 * @param transaction informations about the transaction, optional
	 * 
	 * @return number of moved files
	 * 
	 * @throws PersistenceException error at data layer
	 * @throws IOException I/O error
	 */
	public int enforceFilesIntoFolderStorage(long rootFolderId, DocumentHistory transaction)
			throws PersistenceException, IOException;

	/**
	 * Merges a set of documents into a single PDF file
	 * 
	 * @param documents the list of documents to merge(the order counts)
	 * @param targetFolderId identifier of the target folder
	 * @param fileName name of the output file(must ends with .pdf)
	 * @param transaction informations about the transaction, optional
	 * 
	 * @return the generated merged document
	 * 
	 * @throws IOException I/O error
	 * @throws PersistenceException error at data layer
	 */
	public Document merge(Collection<Document> documents, long targetFolderId, String fileName,
			DocumentHistory transaction) throws IOException, PersistenceException;

	/**
	 * Counts the number of pages of a document
	 * 
	 * @param doc the document
	 * 
	 * @return the number of pages
	 */
	public int countPages(Document doc);
}