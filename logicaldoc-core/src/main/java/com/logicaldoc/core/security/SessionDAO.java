package com.logicaldoc.core.security;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for <code>UserSession</code> handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public interface SessionDAO extends PersistentObjectDAO<Session> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static SessionDAO get() {
		return Context.get(SessionDAO.class);
	}
	
	/**
	 * Removes all the sessions that refers to the current node
	 */
	public void deleteCurrentNodeSessions();

	/**
	 * Counts the number of sessions.
	 * 
	 * @param tenantId The tenant (optional)
	 * @param status The current status (optional)
	 *
	 * @return the number of sessions
	 */
	public int countSessions(Long tenantId, Integer status);

	/**
	 * Counts the number of sessions.
	 * 
	 * @param username The username owning the session
	 * @param status The current status (optional)
	 *
	 * @return the number of sessions
	 */
	public int countSessions(String username, Integer status);

	/**
	 * Retrieves the session of the given SID
	 * 
	 * @param sid identifier of the session
	 * 
	 * @return the session
	 */
	public Session findBySid(String sid);

	/**
	 * Retrieves the session of the given node
	 * 
	 * @param node the node
	 * 
	 * @return the list of sessions
	 */
	public List<Session> findByNode(String node);

	/**
	 * This method deletes all the session entries oldest than the given days
	 * since now. If <code>ttl</code> is 0 or -1, the deletion is not made.
	 * 
	 * @param ttl The maximum number of days over which the session is
	 *        considered old
	 */
	public void cleanOldSessions(int ttl);
}