package com.logicaldoc.core.security.apikey;

import java.security.NoSuchAlgorithmException;
import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for {@link ApiKey} handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public interface ApiKeyDAO extends PersistentObjectDAO<ApiKey> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static ApiKeyDAO get() {
		return Context.get(ApiKeyDAO.class);
	}
	
	/**
	 * Finds the user's ApiKey with a given name
	 * 
	 * @param name the name of the ApiKey
	 * @param userId the identifier of the user
	 * 
	 * @return The unique matching ApiKey
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public ApiKey findByName(String name, long userId) throws PersistenceException;

	/**
	 * Finds the ApiKey by the unique key
	 * 
	 * @param key the key to find
	 * 
	 * @return The unique matching ApiKey
	 * 
	 * @throws PersistenceException Error in the database
	 * @throws NoSuchAlgorithmException Error when encripting the key
	 */
	public ApiKey findByKey(String key) throws PersistenceException, NoSuchAlgorithmException;
	
	
	/**
	 * Finds the user's ApiKeys for a given user
	 * 
	 * @param userId the identifier of the user
	 * 
	 * @return The list of api keys ordered by name
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<ApiKey> findByUser(long userId) throws PersistenceException;
}