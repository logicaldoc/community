package com.logicaldoc.core.document.dao;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentHistory;

/**
 * DAO for <code>DocumentHistory</code> handling.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 */
public interface DocumentHistoryDAO extends PersistentObjectDAO<DocumentHistory> {
	/**
	 * This method selects all histories of a given document.
	 * 
	 * @param docId - ID of the document.
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findByDocId(long docId);

	/**
	 * This method selects all histories of a given document.
	 * 
	 * @param docId - ID of the document.
	 * @param event - Optional event code
	 * 
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findByDocIdAndEvent(long docId, String event);

	/**
	 * This method selects all histories of a given user.
	 * 
	 * @param userId identifier of the user
	 * 
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findByUserId(long userId);

	/**
	 * This method selects all histories of a given folder.
	 * 
	 * @param folderId - ID of the document.
	 * 
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findByFolderId(long folderId);

	/**
	 * This method finds all histories about a path (you can use expression)
	 * 
	 * @param pathExpression The path expression (like /Default/acme%)
	 * @param oldestDate The older date for the retrieved histories
	 * @param events events Optional list of event codes to be used as filter
	 * 
	 * @param max Optional maximum number of records
	 * 
	 * @return list of histories ordered by date asc
	 */
	public List<DocumentHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
			Integer max);

	/**
	 * This method selects all histories not notified yet.
	 * 
	 * @param max Optional maximum number of records
	 * 
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findNotNotified(Integer max);

	/**
	 * This method deletes all the document history entries oldest than the
	 * given days from now. If <code>ttl</code> is 0 or -1, the deletion is not
	 * made.
	 * 
	 * @param ttl The maximum number of days over which the history is
	 *        considered old
	 */
	public void cleanOldHistories(int ttl);

	/**
	 * Marks all the histories of a specific event as read by the specified
	 * user.
	 * 
	 * @param event the event name
	 * @param userId identifier of the user
	 */
	public void markHistoriesAsRead(String event, long userId);

	/**
	 * This method selects all histories of a given user and related to the
	 * given event.
	 * 
	 * @param userId The user identifier
	 * @param sessionId The session identifier (optional)
	 * @param event Code of the event (optional)
	 * 
	 * @return list of histories ordered by date
	 */
	public List<DocumentHistory> findByUserIdAndEvent(long userId, String event, String sessionId);
}