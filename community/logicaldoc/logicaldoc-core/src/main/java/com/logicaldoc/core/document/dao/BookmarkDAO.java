package com.logicaldoc.core.document.dao;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.Bookmark;

/**
 * DAO service for bookmarks
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public interface BookmarkDAO extends PersistentObjectDAO<Bookmark> {

	/**
	 * Finds all bookmarks for the given user id.
	 * 
	 * @param userId ID of the user.
	 * @return Collection of all bookmarks for the specified user ordered by
	 *         position.
	 */
	public List<Bookmark> findByUserId(long userId);

	/**
	 * Finds the identifiers of the docs bookmarked by the given user.
	 */
	public List<Long> findBookmarkedDocs(long userId);

	/**
	 * Checks if the document is bookmarked
	 */
	public boolean isDocBookmarkedByUser(long docId, long userId);

	/**
	 * Finds all bookmarks for the given user id and the given document id.
	 * 
	 * @param userId ID of the user.
	 * @param docId ID of the document.
	 * @return The bookmark
	 */
	public Bookmark findByUserIdAndDocId(long userId, long docId);

	/**
	 * Finds the bookmark for the given user id and the given folder id.
	 * 
	 * @param userId ID of the user.
	 * @param folderId ID of the folder.
	 * @return The bookmark
	 */
	public Bookmark findByUserIdAndFolderId(long userId, long folderId);
}
