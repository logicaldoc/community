package com.logicaldoc.core.security;

import java.util.Map;

/**
 * This interface defines hooks called before and after a particular event
 * occurs on the specified user.
 * <p>
 * Each methods has access to a dictionary map that can be used through the
 * execution pipeline in order to carry needed informations among all listeners.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 5.1
 */
public interface UserListener {
	/**
	 * Called before a user is stored in the database
	 * 
	 * @param user The user to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws Exception
	 */
	public void beforeStore(User user, UserHistory transaction, Map<String, Object> dictionary) throws Exception;

	/**
	 * Called after a user is stored in the database
	 * 
	 * @param user The user to be stored
	 * @param transaction Transaction informations
	 * @param dictionary Dictionary of the execution pipeline
	 * 
	 * @throws Exception
	 */
	public void afterStore(User user, UserHistory transaction, Map<String, Object> dictionary) throws Exception;
}