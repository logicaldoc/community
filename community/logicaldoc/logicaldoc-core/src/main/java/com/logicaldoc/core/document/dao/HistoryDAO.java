package com.logicaldoc.core.document.dao;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.History;

/**
 * DAO for <code>History</code> handling.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 */
public interface HistoryDAO extends PersistentObjectDAO<History> {
	/**
	 * This method selects all histories of a given document.
	 * 
	 * @param docId - ID of the document.
	 * @return list of histories ordered by date
	 */
	public List<History> findByDocId(long docId);

	/**
	 * This method selects all histories of a given user.
	 * 
	 * @param userId
	 * @return list of histories ordered by date
	 */
	public List<History> findByUserId(long userId);

	/**
	 * This method selects all histories of a given folder.
	 * 
	 * @param folderId - ID of the document.
	 * @return list of histories ordered by date
	 */
	public List<History> findByFolderId(long folderId);

	/**
	 * This method finds all histories about a path (you can use expression)
	 * 
	 * @param pathExpression The path expression (like /Default/acme%)
	 * @param oldestDate The older date for the retrieved histories
	 * @events events Optional list of event codes to be used as filter
	 * @param max Optional maximum number of records
	 * @return
	 */
	public List<History> findByPath(String pathExpression, Date oldestDate, Collection<String> events, Integer max);

	/**
	 * This method selects all histories not notified yet.
	 * 
	 * @return max Optional maximum number of records
	 * @return list of histories ordered by date
	 */
	public List<History> findNotNotified(Integer max);

	/**
	 * This method deletes all the document history entries oldest than the
	 * given days from now. If <code>ttl</code> is 0 or -1, the cancellation is
	 * not made.
	 * 
	 * @param ttl The maximum number of days over which the history is
	 *        considered old
	 */
	public void cleanOldHistories(int ttl);

	/**
	 * This method selects all histories of a given user and related to the
	 * given event.
	 * 
	 * @param userId The user identifier
	 * @param sessionId The session identifier (optional)
	 * @param event The string represtation of history event (optional)
	 * @return list of histories ordered by date
	 */
	public List<History> findByUserIdAndEvent(long userId, String event, String sessionId);
}