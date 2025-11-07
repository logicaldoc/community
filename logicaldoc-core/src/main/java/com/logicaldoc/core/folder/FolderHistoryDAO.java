package com.logicaldoc.core.folder;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for {@link FolderHistory}s handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 5.0
 */
public interface FolderHistoryDAO extends PersistentObjectDAO<FolderHistory> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static FolderHistoryDAO get() {
		return Context.get(FolderHistoryDAO.class);
	}

	/**
	 * This method selects all histories of a given user.
	 * 
	 * @param userId identifier of the user
	 * 
	 * @return list of histories ordered by date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<FolderHistory> findByUserId(long userId) throws PersistenceException;

	/**
	 * This method selects all histories of a given folder.
	 * 
	 * @param folderId ID of the folder
	 * 
	 * @return list of histories ordered by date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<FolderHistory> findByFolderId(long folderId) throws PersistenceException;

	/**
	 * This method selects all histories of a given folder and event occurred
	 * after a given date.
	 * 
	 * @param folderId ID of the folder
	 * @param event code of the event to search
	 * @param oldestDate optional oldest date
	 * 
	 * @return list of histories ordered by date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<FolderHistory> findByFolderIdAndEvent(long folderId, String event, Date oldestDate)
			throws PersistenceException;

	/**
	 * This method selects all histories not notified yet.
	 * 
	 * @param max Optional maximum number of records
	 * 
	 * @return list of histories ordered by date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<FolderHistory> findNotNotified(Integer max) throws PersistenceException;

	/**
	 * This method selects all histories of a given user and related to the
	 * given event.
	 * 
	 * @param userId The user identifier
	 * @param event The history event
	 * 
	 * @return list of histories ordered by date
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<FolderHistory> findByUserIdAndEvent(long userId, String event) throws PersistenceException;

	/**
	 * This method finds all histories about a path (you can use expression)
	 * 
	 * @param pathExpression The path expression (like /Default/acme%)
	 * @param oldestDate The older date for the retrieved histories
	 * @param events Optional list of event codes to be used as filter
	 * @param max Optional maximum number of records
	 * 
	 * @return The list of histories that matched the given criteria
	 * @throws PersistenceException Error in the data layer
	 */
	public List<FolderHistory> findByPath(String pathExpression, Date oldestDate, Collection<String> events,
			Integer max) throws PersistenceException;

	/**
	 * This method deletes all the user history entries oldest than the given
	 * days from now. If <code>ttl</code> is 0 or -1, the cancellation is not
	 * made.
	 * 
	 * @param ttl The maximum number of days over which the history is
	 *        considered old
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void cleanOldHistories(int ttl) throws PersistenceException;
}