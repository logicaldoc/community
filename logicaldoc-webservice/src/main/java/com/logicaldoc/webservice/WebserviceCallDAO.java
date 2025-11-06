package com.logicaldoc.webservice;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for {@link WebserviceCall} handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public interface WebserviceCallDAO extends PersistentObjectDAO<WebserviceCall> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static WebserviceCallDAO get() {
		return Context.get(WebserviceCallDAO.class);
	}
	
	/**
	 * This method deletes all the chat entries oldest than the given days from
	 * now. If <code>ttl</code> is 0 or -1, the cancellation is not made
	 * 
	 * @param ttl The maximum number of days over which the item is considered
	 *        old
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void cleanOldCalls(int ttl) throws PersistenceException;
}