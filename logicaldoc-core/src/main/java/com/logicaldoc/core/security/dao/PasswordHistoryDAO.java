package com.logicaldoc.core.security.dao;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.PasswordHistory;

/**
 * This class is a DAO-service for PasswordHistory-objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public interface PasswordHistoryDAO extends PersistentObjectDAO<PasswordHistory> {

	/**
	 * Gets the password used by the user in last <b>max</b> times
	 * 
	 * @param userId Identifier of the user
	 * @param password The password to check
	 * @param max number of most recent records to evaluate
	 * 
	 * @return the found history record, if any
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public PasswordHistory findByUserIdAndPassword(long userId, String password, int max) throws PersistenceException;
	
	/**
	 * Gets all the histories related to a given user ordered by date desc
	 * 
	 * @param userId Identifier of the user
	 * @param max maximum number of returned records
	 * 
	 * @return orderer list of histories
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<PasswordHistory> findByUserId(long userId, Integer max) throws PersistenceException;

	/**
	 * This method deletes all the histories maintaining the most recent ones.
	 * If <code>retain</code> is 0 or -1, the cancellation is not made.
	 * 
	 * @param userId identifier of the user
	 * @param retain the number of most recent histories to maintain
	 */
	public void cleanOldHistories(long userId, int retain);
}