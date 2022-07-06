package com.logicaldoc.webservice;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * DAO for {@link WebserviceCall} handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public interface WebserviceCallDAO extends PersistentObjectDAO<WebserviceCall> {

	/**
	 * This method deletes all the chat entries oldest than the given days from
	 * now. If <code>ttl</code> is 0 or -1, the cancellation is not made
	 * 
	 * @param ttl The maximum number of days over which the item is considered
	 *        old
	 */
	public void cleanOldCalls(int ttl);
}