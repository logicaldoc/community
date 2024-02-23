package com.logicaldoc.core.document;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;

/**
 * DAO service for bookmarks
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public interface BookmarkDAO extends PersistentObjectDAO<Bookmark> {

	/**
	 * Finds all bookmarks for the given user id
	 * 
	 * @param userId ID of the user
	 * 
	 * @return Collection of all bookmarks for the specified user ordered by
	 *         position
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<Bookmark> findByUserId(long userId) throws PersistenceException;

	/**
	 * Finds the identifiers of the docs bookmarked by the given user
	 * 
	 * @param userId identifier of the user
	 * 
	 * @return list of document identifiers
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<Long> findBookmarkedDocs(long userId) throws PersistenceException;

	/**
	 * Checks if the document is bookmarked by a user
	 * 
	 * @param docId identifier of the document
	 * @param userId identifier of the user
	 * 
	 * @return true id the document was bookmarked
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public boolean isDocBookmarkedByUser(long docId, long userId) throws PersistenceException;

	/**
	 * Finds all bookmarks for the given user id and the given document's
	 * identifier
	 * 
	 * @param userId ID of the user
	 * @param docId ID of the document
	 * 
	 * @return The bookmark
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Bookmark findByUserIdAndDocId(long userId, long docId) throws PersistenceException;

	/**
	 * Finds the bookmark for the given user id and the given folder id
	 * 
	 * @param userId ID of the user
	 * @param folderId ID of the folder
	 * 
	 * @return The bookmark
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Bookmark findByUserIdAndFolderId(long userId, long folderId) throws PersistenceException;
}
