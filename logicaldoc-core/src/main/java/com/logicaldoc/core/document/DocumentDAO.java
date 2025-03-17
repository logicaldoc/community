package com.logicaldoc.core.document;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.Permission;

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
	 * @return Document with given ID
	 * 
	 * @throws PersistenceException an error happened in the database
	 */
	public Document findDocument(long docId) throws PersistenceException;

	/**
	 * This method finds a document by the custom ID.
	 * 
	 * @param customId custom ID of the document.
	 * @param tenantId ID of the contained tenant.
	 * 
	 * @return Document with given ID.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Document findByCustomId(String customId, long tenantId) throws PersistenceException;

	/**
	 * Finds all documents for a user.
	 * 
	 * @param userId ID of the user.
	 * @return Collection of all documentId required for the specified user.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findByUserId(long userId) throws PersistenceException;

	/**
	 * Finds all document ids inside the given folder.
	 * 
	 * @param folderId Folder identifier
	 * @param max Optional, maximum number of returned elements
	 * 
	 * @return Collection of all document id in the folder.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findDocIdByFolder(long folderId, Integer max) throws PersistenceException;

	/**
	 * Finds all documents direct children of the given folder.
	 * 
	 * @param folderId Folder identifier
	 * @param max Optional, defines the maximum records number
	 * 
	 * @return Collection of all documents in the folder
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findByFolder(long folderId, Integer max) throws PersistenceException;

	/**
	 * Finds all document of0 the specified status and locked by the specified
	 * user
	 * 
	 * @param userId The user id(optional)
	 * @param status The status code(optional)
	 * 
	 * @return Collection of all Documents locked by the specified user and of
	 *         the specified status.
	 */
	public List<Document> findByLockUserAndStatus(Long userId, Integer status);

	/**
	 * Finds a max number of documents last modified by an user.
	 * 
	 * @param userId ID of the user.
	 * @param max maximum number of returned elements
	 * 
	 * @return Collection of the last documents changed by the specified user.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findLastModifiedByUserId(long userId, int max) throws PersistenceException;

	/**
	 * Finds the last downloaded documents by the given user
	 * 
	 * @param userId id of the user
	 * @param max maximum number of returned elements
	 * 
	 * @return list of documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findLastDownloadsByUserId(long userId, int max) throws PersistenceException;

	/**
	 * Finds all documents whose id is included in the given pool of ids
	 * 
	 * @param ids identifiers of the documents
	 * @param max Optional, maximum number of returned elements
	 * 
	 * @return list of documents
	 */
	public List<Document> findByIds(Set<Long> ids, Integer max);

	/**
	 * This method finds all Doc Ids by a tag.
	 * 
	 * @param tag Tag of the document.
	 * 
	 * @return Document with specified tag.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findDocIdByTag(String tag) throws PersistenceException;

	/**
	 * Selects all tags and counts the occurrences.
	 * 
	 * @param firstLetter the first letter
	 * @param tenantId identifier of the tenant
	 *
	 * @return the map tag - count
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Map<String, Long> findTags(String firstLetter, Long tenantId) throws PersistenceException;

	/**
	 * Searches for all tags,
	 * 
	 * @param firstLetter Optional first letter hint
	 * @param tenantId ID of the tenant to search in
	 * 
	 * @return The list of all tags in the system
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<String> findAllTags(String firstLetter, Long tenantId) throws PersistenceException;

	/**
	 * Finds authorized documents for a user having a specified tag.
	 * 
	 * @param userId ID of the user
	 * @param tag Tag of the document
	 * @param max Optional, defines the maximum records number
	 * 
	 * @return Collection of found documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findByUserIdAndTag(long userId, String tag, Integer max) throws PersistenceException;

	/**
	 * Finds authorized documents ids for a user having a specified tag.
	 * 
	 * @param userId ID of the user.
	 * @param tag Tag of the document
	 * 
	 * @return Set of found ids.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findDocIdByUserIdAndTag(long userId, String tag) throws PersistenceException;

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
	 * 
	 * @return The collection of linked documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findLinkedDocuments(long docId, String linkType, Integer direction)
			throws PersistenceException;

	/**
	 * Finds that document that lies under a specific folder (given by the id)
	 * an with a given fileName(like operator is used)
	 * 
	 * @param folderId The folder id (it can be null).
	 * @param fileName name of the file or a part of it (you can use SQL % jolly
	 *        chars, eg: contract.pdf, %ontrac%)
	 * @param excludeId Optional id of a document that must not be considered
	 * @param tenantId Optional id of the tenant
	 * @param max Optional maximum number of returned elements
	 * 
	 * @return The list of documents with the given fileName. If the folder id
	 *         is null, the searched document can belong to any folder in the
	 *         repository
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findByFileNameAndParentFolderId(Long folderId, String fileName, Long excludeId, Long tenantId,
			Integer max) throws PersistenceException;

	/**
	 * Finds a document by it's full path
	 * 
	 * @param path The path comprehensive of the file name
	 * @param tenantId The tenant
	 * 
	 * @return the found document
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Document findByPath(String path, long tenantId) throws PersistenceException;

	/**
	 * Initializes lazy loaded collections
	 * 
	 * @param doc The document to be initialized
	 */
	public void initialize(Document doc);

	/**
	 * Obtains the total size of the repository, that is the sum of sizes of all
	 * documents and their versions
	 * 
	 * @param tenantId identifier of the tenant(optional)
	 * @param publisherId identifier of the publisher user (optional)
	 * @param computeDeleted If true, even deleted documents are considered
	 * 
	 * @return the total size expressed in bytes
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public long computeTotalSize(Long tenantId, Long publisherId, boolean computeDeleted) throws PersistenceException;

	/**
	 * Gets the collection of deleted document ids
	 * 
	 * @return collection of document identifiers
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findDeletedDocIds() throws PersistenceException;

	/**
	 * Finds the list of deleted documents.
	 * <p>
	 * <b>Attention:</b> The returned objects are not fully operative and are
	 * populated with a minimal set of data.
	 * </p>
	 * 
	 * @return the list of documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findDeletedDocs() throws PersistenceException;

	/**
	 * Counts the number of documents
	 * 
	 * @param tenantId The tenant to search in
	 * @param computeDeleted If true, even deleted documents are considered
	 * @param computeArchived If true, even archived documents are considered
	 * 
	 * @return number of documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public long count(Long tenantId, boolean computeDeleted, boolean computeArchived) throws PersistenceException;

	/**
	 * Finds all documents by the indexed state. Order by ascending lastModifed
	 * 
	 * @param indexed the indexed property
	 * @return Collection of all documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findByIndexed(int indexed) throws PersistenceException;

	/**
	 * Counts the number of documents in a given indexation status(@see
	 * {@link AbstractDocument#getIndexed()}
	 * 
	 * @param indexed the indexation status to check
	 * 
	 * @return number of documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public long countByIndexed(int indexed) throws PersistenceException;

	/**
	 * Restores a previously deleted document
	 * 
	 * @param docId Id of the document to be restored
	 * @param folderId Id of the folder the document will be restored into
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void restore(long docId, long folderId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Restores a previously archived document
	 * 
	 * @param docId Ids of the document to be restored
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void unarchive(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Marks the document, with the given docId, as immutable. Unlocks the
	 * document if it was locked.
	 * 
	 * @param docId identifier of the document
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void makeImmutable(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Shortcut for deleteAll(documents, 1, transaction)
	 * 
	 * @param documents the documents
	 * @param transaction the current session
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void deleteAll(Collection<Document> documents, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Deletes all documents form the database and modifies the custom ids of
	 * all documents
	 * 
	 * @param documents The documents to be deleted
	 * @param delCode The deletion code
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void deleteAll(Collection<Document> documents, int delCode, DocumentHistory transaction)
			throws PersistenceException;

	/**
	 * This method persists the document object and insert a new document
	 * history entry
	 * 
	 * @param doc the document
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void store(Document doc, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method deletes the document object and insert a new document history
	 * entry.
	 * 
	 * @param docId The id of the document to delete
	 * @param delCode The deletion code
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void delete(long docId, int delCode, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Shortcut for delete(docId, 1, transaction)
	 * 
	 * @param docId identifier of the document
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void delete(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Archives a document
	 * 
	 * @param docId identifier of the document
	 * @param transaction entry to log the event
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void archive(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Finds archived documents in a folder (direct childeren only)
	 * 
	 * @param folderId identifier of the folder
	 * 
	 * @return list of documents
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findArchivedByFolder(long folderId) throws PersistenceException;

	/**
	 * Gets the ids of all aliases associated to the document with the given
	 * docId
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return list of identifiers of aliases
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Long> findAliasIds(long docId) throws PersistenceException;

	/**
	 * Finds all deleted docs of a specific user.
	 * 
	 * @param userId The user that performed the deletion
	 * @param max Optional, defines the max number of returned elements
	 * 
	 * @return The documents list
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<Document> findDeleted(long userId, Integer max) throws PersistenceException;

	/**
	 * This method deletes the documents into deleted folders.
	 * 
	 * @param deleteUserId The id of the user that performs the deleting
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void deleteOrphaned(long deleteUserId) throws PersistenceException;

	/**
	 * Finds all document ids inside the specified folders that are published in
	 * the current date.
	 * 
	 * @param folderIds Set of folder ids in which the method will search
	 * 
	 * @return List of published document ids
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Collection<Long> findPublishedIds(Collection<Long> folderIds) throws PersistenceException;

	/**
	 * Updates the document's digest (SHA-1)
	 * 
	 * @param doc The document to be processed
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void updateDigest(Document doc) throws PersistenceException;

	/**
	 * Cleans all references to expired transactions. If no lock is found for a
	 * document referencing a given transaction, the transactionId will be set
	 * to null.
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void cleanExpiredTransactions() throws PersistenceException;

	/**
	 * Saves a document's history
	 * 
	 * @param doc the document
	 * 
	 * @param transaction the current session
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void saveDocumentHistory(Document doc, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Cleans the ld_uniquetag table removing no more used tags, this method is
	 * optimized and uses a couple of prepared statements, in case of error it
	 * switches to {@link DocumentDAO#cleanUnexistingUniqueTagsOneByOne()}
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void cleanUnexistingUniqueTags() throws PersistenceException;

	/**
	 * Cleans the ld_uniquetag table removing no more used tags programmatically
	 * one by one. This method is not as optimized as
	 * {@link DocumentDAO#cleanUnexistingUniqueTags()} and should not be invoked
	 * directly
	 * 
	 * @throws PersistenceException error at data layer
	 */
	void cleanUnexistingUniqueTagsOneByOne() throws PersistenceException;

	/**
	 * Puts into ld_uniquetag the new unique tags
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void insertNewUniqueTags() throws PersistenceException;

	/**
	 * Updates the count of the unique tags
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void updateCountUniqueTags() throws PersistenceException;

	/**
	 * Gets the tag cloud for the given tenant
	 * 
	 * @param tenantId identifier of the tenant
	 * @param max maximum number of returned elements
	 * 
	 * @return list of tag clouds
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<TagCloud> getTagCloud(long tenantId, int max) throws PersistenceException;

	/**
	 * Gets the tag cloud for the given tenant
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return list of tag clouds
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<TagCloud> getTagCloud(String sid) throws PersistenceException;

	/**
	 * Retrieves, the workspace where the document(or alias) is stored
	 * 
	 * @param docId identifier of a document
	 * 
	 * @return the parent workspace
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Folder getWorkspace(long docId) throws PersistenceException;

	/**
	 * Protects the document with a password. The same password is replicated to
	 * all the versions
	 * 
	 * @param docId ID of the document
	 * @param password The new password in clear
	 * @param transaction history informations
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void setPassword(long docId, String password, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Removes the password protection from the document. The same action is
	 * replicated to all the versions
	 * 
	 * @param docId ID of the document
	 * @param transaction session informations
	 *
	 * @throws PersistenceException error at data layer
	 */
	public void unsetPassword(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * Retrieves the list of duplicated checksums
	 * 
	 * @param tenantId identifier of the tenant
	 * @param folderId identifier of the folder
	 * 
	 * @return list of duplicated digests
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<String> findDuplicatedDigests(Long tenantId, Long folderId) throws PersistenceException;

	/**
	 * Retrieves the alphabetically ordered list of all the document's tags
	 * 
	 * @param docId identifier of the document
	 * 
	 * @return list of the document's tags
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public List<String> findTags(long docId) throws PersistenceException;

	/**
	 * Replaces the document's security policies with those from the parent
	 * folder
	 * 
	 * @param docId identifier of the document
	 * @param transaction the current transaction
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public void applyParentFolderSecurity(long docId, DocumentHistory transaction) throws PersistenceException;

	/**
	 * This method is looking up for writing rights for a document and an user.
	 * 
	 * @param documentId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return if the write permission is granted
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public boolean isWriteAllowed(long documentId, long userId) throws PersistenceException;

	public boolean isReadAllowed(long documentId, long userId) throws PersistenceException;
	
	public boolean isPreviewAllowed(long documentId, long userId) throws PersistenceException;

	public boolean isPrintAllowed(long documentId, long userId) throws PersistenceException;

	public boolean isDownloadAllowed(long documentId, long userId) throws PersistenceException;

	public boolean isMoveAllowed(long documentId, long userId) throws PersistenceException;

	/**
	 * This method checks if the given permission is enabled for a document and
	 * an user.
	 * 
	 * @param permission the permission to check
	 * @param documentId ID of the folder
	 * @param userId ID of the user
	 * 
	 * @return if the permission is granted to the user
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public boolean isPermissionAllowed(Permission permission, long documentId, long userId) throws PersistenceException;

	/**
	 * Finds all permissions of a user enabled on the specified document
	 * 
	 * @param documentId ID of the document
	 * @param userId ID of the user
	 * 
	 * @return Collection of enabled permissions
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Set<Permission> getAllowedPermissions(long documentId, long userId) throws PersistenceException;
}