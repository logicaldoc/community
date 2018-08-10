package com.logicaldoc.core.document.dao;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.History;
import com.logicaldoc.core.document.TagCloud;
import com.logicaldoc.core.folder.Folder;

/**
 * This class is a DAO-service for documents.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 * @version 1.0
 */
public interface DocumentDAO extends PersistentObjectDAO<Document> {

	/**
	 * This method finds a document by the ID and if it is an alias the
	 * referenced document is returned instead.
	 * 
	 * @param docId ID of the document(or the alias)
	 * 
	 * @return Document with given ID.
	 */
	public Document findDocument(long docId);

	/**
	 * This method finds a document by the custom ID.
	 * 
	 * @param customId custom ID of the document.
	 * @param tenantId ID of the contained tenant.
	 * 
	 * @return Document with given ID.
	 */
	public Document findByCustomId(String customId, long tenantId);

	/**
	 * Finds all documents for an user.
	 * 
	 * @param userId ID of the user.
	 * @return Collection of all documentId required for the specified user.
	 */
	public List<Long> findByUserId(long userId);

	/**
	 * Finds all document ids inside the given folder.
	 * 
	 * @param folderId Folder identifier
	 * @return Collection of all document id in the folder.
	 */
	public List<Long> findDocIdByFolder(long folderId, Integer max);

	/**
	 * Finds all documents direct children of the given folder.
	 * 
	 * @param folderId Folder identifier
	 * @return Collection of all documents in the folder.
	 */
	public List<Document> findByFolder(long folderId, Integer max);

	/**
	 * Finds all document of the specified status and locked by the specified
	 * user
	 * 
	 * @param userId The user id(optional)
	 * @param status The status code(optional)
	 * @return Collection of all Documents locked by the specified user and of
	 *         the specified status.
	 */
	public List<Document> findByLockUserAndStatus(Long userId, Integer status);

	/**
	 * Finds a max number of documents last modified by an user.
	 * 
	 * @param userId ID of the user.
	 * @return Collection of the last documents changed by the specified user.
	 */
	public List<Document> findLastModifiedByUserId(long userId, int maxResults);

	/**
	 * Finds the last downloaded documents by the given user
	 * 
	 * @param userId id of the user
	 * @param maxResults maximum number of returned elements
	 * @return
	 */
	public List<Document> findLastDownloadsByUserId(long userId, int maxResults);

	/**
	 * Finds all documents whose id is included in the given pool of ids
	 */
	public List<Document> findByIds(Long[] ids, Integer max);

	/**
	 * This method finds all Doc Ids by a tag.
	 * 
	 * @param tag Tag of the document.
	 * @return Document with specified tag.
	 */
	public List<Long> findDocIdByTag(String tag);

	/**
	 * This method selects all tags and counts the occurrences.
	 */
	public Map<String, Long> findTags(String firstLetter, Long tenantId);

	/**
	 * Searches for all tags,
	 * 
	 * @param firstLetter Optional first letter hint
	 * @param tenantId ID of the tenant to search in
	 * 
	 * @return The list of all tags in the system
	 */
	public List<String> findAllTags(String firstLetter, Long tenantId);

	/**
	 * Finds authorized documents for a user having a specified tag.
	 * 
	 * @param userId ID of the user
	 * @param tag Tag of the document
	 * @param max Optional, defines the maximum records number
	 * @return Collection of found documents
	 */
	public List<Document> findByUserIdAndTag(long userId, String tag, Integer max);

	/**
	 * Finds authorized documents ids for a user having a specified tag.
	 * 
	 * @param userId ID of the user.
	 * @param tag Tag of the document
	 * @return Set of found ids.
	 */
	public List<Long> findDocIdByUserIdAndTag(long userId, String tag);

	/**
	 * This method enlists documents linked to the given document.
	 * <p>
	 * <b>Important:</b> The attribute <code>direction</code> defines the search
	 * logic as follows:
	 * <ul>
	 * <li>1: docId will be compared to link's document1</li>
	 * <li>2: docId will be compared to link's document2</li>
	 * <li>null: docId will be compared to both document1 and document2</li>
	 * </ul>
	 * 
	 * @param docId All documents linked to this one will be searched
	 * @param linkType Type of the link (optional)
	 * @param direction if 1 docId will be compared to link's document1, id 2
	 *        docId will be compared to link's document2, if null docId will be
	 *        compared to both document1 and document2 of the link.
	 * @return The collection of linked documents
	 */
	public List<Document> findLinkedDocuments(long docId, String linkType, Integer direction);

	/**
	 * Finds that document that lies under a specific folder (given by the id)
	 * an with a given fileName(like operator is used)
	 * 
	 * @param folderId The folder id (it can be null).
	 * @param fileName
	 * @param excludeId Optional id of a document that must not be considered
	 * @param tenantId Optional id of the tenant
	 * @param max Optional maximum number of returned elements
	 * @return The list of documents with the given fileName. If the folder id
	 *         is null, the searched document can belong to any folder in the
	 *         repository.
	 */
	public List<Document> findByFileNameAndParentFolderId(Long folderId, String fileName, Long excludeId,
			Long tenantId, Integer max);

	/**
	 * Finds a document by it's full path
	 * 
	 * @param path The path comprehensive of the file name
	 * @param tenantId The tenant
	 * 
	 * @return the found document
	 */
	public Document findByPath(String path, long tenantId);

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param doc The document to be initialized
	 */
	public void initialize(Document doc);

	/**
	 * Obtains the total size of the archive, that is the sum of sizes of all
	 * documents
	 * 
	 * @param computeDeleted If true, even deleted documents are considered
	 * @return
	 */
	public long getTotalSize(boolean computeDeleted);

	/**
	 * Gets the collection of deleted document ids
	 */
	public List<Long> findDeletedDocIds();

	/**
	 * Finds the list of deleted documents.
	 * <p>
	 * <b>Attention:</b> The returned objects are not fully operative and are
	 * populated with a minimal set of data.
	 */
	public List<Document> findDeletedDocs();

	/**
	 * Counts the number of documents
	 * 
	 * @param tenantId The tenant to search in
	 * @param computeDeleted If true, even deleted documents are considered
	 * @param computeArchived If true, even archived documents are considered
	 */
	public long count(Long tenantId, boolean computeDeleted, boolean computeArchived);

	/**
	 * Finds all documents by the indexed state. Order by ascending lastModifed
	 * 
	 * @param indexed the indexed property
	 * @return Collection of all documents
	 */
	public List<Document> findByIndexed(int indexed);

	/**
	 * Counts the number of documents indexed or not
	 */
	public long countByIndexed(int indexed);

	/**
	 * Restores a previously deleted document
	 * 
	 * @param docId Id of the document to be restored
	 * @param folderId Id of the folder the document will be restored into
	 * @param transaction entry to log the event
	 */
	public void restore(long docId, long folderId, History transaction);

	/**
	 * Restores a previously archived document
	 * 
	 * @param docId Ids of the document to be restored
	 * @param transaction entry to log the event
	 */
	public void unarchive(long docId, History transaction);

	/**
	 * Marks the document, with the given docId, as immutable. Unlocks the
	 * document if it was locked.
	 * 
	 * @param docId
	 * @param transaction entry to log the event
	 */
	public void makeImmutable(long docId, History transaction);

	/**
	 * Shortcut for deleteAll(documents, 1, transaction
	 */
	public void deleteAll(Collection<Document> documents, History transaction);

	/**
	 * Deletes all documents form the database and modifies the custom ids of
	 * all documents
	 * 
	 * @param documents The documents to be deleted
	 * @param delCode The deletion code
	 * @param transaction entry to log the event
	 */
	public void deleteAll(Collection<Document> documents, int delCode, History transaction);

	/**
	 * This method persists the document object and insert a new document
	 * history entry.
	 * 
	 * @param doc
	 * @param transaction entry to log the event
	 * @return True if successfully stored in a database.
	 */
	public boolean store(final Document doc, final History transaction);

	/**
	 * This method deletes the document object and insert a new document history
	 * entry.
	 * 
	 * @param docId The id of the document to delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event
	 * @return True if successfully deleted from the database.
	 */
	public boolean delete(long docId, int delCode, History transaction);

	/**
	 * Shortcut for delete(docId, 1, transaction)
	 */
	public boolean delete(long docId, History transaction);

	/**
	 * Archives a document
	 */
	public boolean archive(long docId, History transaction);

	/**
	 * Finds archived documents in a folder (direct childeren only)
	 */
	public List<Document> findArchivedByFolder(long folderId);

	/**
	 * Gets the ids of all aliases associated to the document with the given
	 * docId
	 * 
	 * @param docId The document Id
	 */
	public List<Long> findAliasIds(long docId);

	/**
	 * Finds all deleted docs of a specific user.
	 * 
	 * @param userId The user that performed the deletion
	 * @param maxHits Optional defines the max number of returned hits
	 * @return The documents list
	 */
	public List<Document> findDeleted(long userId, Integer maxHits);

	/**
	 * This method deletes the documents into deleted folders.
	 * 
	 * @param deleteUserId The id of the user that performs the deleting.
	 * @return True if successfully deleted from the database.
	 */
	public boolean deleteOrphaned(long deleteUserId);

	/**
	 * Finds all document ids inside the specified folders that are published in
	 * the current date.
	 * 
	 * @param folderIds Set of folder ids in which the method will search
	 * @return List of published document ids
	 */
	public Collection<Long> findPublishedIds(Collection<Long> folderIds);

	/**
	 * Updates the document's digest (SHA-1)
	 * 
	 * @param doc The document to be processed
	 */
	public void updateDigest(Document doc);

	/**
	 * Cleans all references to expired transactions. If no lock is found for a
	 * document referencing a given transaction, the transactionId will be set
	 * to null.
	 */
	public void cleanExpiredTransactions();

	/**
	 * Saves a document's history
	 */
	public void saveDocumentHistory(Document doc, History transaction);

	/**
	 * Cleans the ld_uniquetag table removing no more used tags
	 */
	public void cleanUnexistingUniqueTags();

	/**
	 * Puts into ld_uniquetag the new unique tags
	 */
	public void insertNewUniqueTags();

	/**
	 * Updates the count of the unique tags
	 */
	public void updateCountUniqueTags();

	/**
	 * Gets the tag cloud for the given tenant
	 */
	public List<TagCloud> getTagCloud(long tenantId, int maxTags);

	/**
	 * Gets the tag cloud for the given tenant
	 */
	public List<TagCloud> getTagCloud(String sid);

	/**
	 * Retrieves, the workspace where the document(or alias) is stored
	 */
	public Folder getWorkspace(long docId);

	/**
	 * Protects the document with a password. The same password is replicated to
	 * all the versions
	 * 
	 * @param docId ID of the document
	 * @param password The new password in clear
	 * @param transaction history informations
	 * @throws Exception
	 */
	public void setPassword(long docId, String password, History transaction) throws Exception;

	/**
	 * Removes the password protection from the document. The same action is
	 * replicated to all the versions
	 * 
	 * @param docId ID of the document
	 * @param transaction history informations
	 */
	public void unsetPassword(long docId, History transaction);
	
	/**
	 * Retrieves the list of duplicated checksums
	 */
	public List<String> findDuplicatedDigests(Long tenantId, Long folderId);
}