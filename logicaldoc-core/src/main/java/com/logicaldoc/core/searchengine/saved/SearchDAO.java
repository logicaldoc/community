package com.logicaldoc.core.searchengine.saved;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This class is a DAO-service for persistent {@link SavedSearch} objects.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public interface SearchDAO extends PersistentObjectDAO<SavedSearch> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static SearchDAO get() {
		return Context.get(SearchDAO.class);
	}

	/**
	 * Gets the search using the alternate key
	 * 
	 * @param userId Identifier of the user
	 * @param name The name of the search
	 * 
	 * @return the found search gridRecord, if any
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public SavedSearch findByUserIdAndName(long userId, String name) throws PersistenceException;

	/**
	 * Gets all the searches of a given user ordered by name asc
	 * 
	 * @param userId Identifier of the user
	 * 
	 * @return orderer list of searches
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<SavedSearch> findByUserId(long userId) throws PersistenceException;
}