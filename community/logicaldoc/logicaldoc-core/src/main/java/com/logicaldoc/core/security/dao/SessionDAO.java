package com.logicaldoc.core.security.dao;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Session;

/**
 * DAO for <code>UserSession</code> handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public interface SessionDAO extends PersistentObjectDAO<Session> {

	/**
	 * Removes all the sessions that refers to the current node
	 */
	public void deleteCurrentNodeSessions();

	/**
	 * Counts the number of sessions.
	 * 
	 * @param tenantId The tenant (optional)
	 * @param status The current status (optional)
	 * @return the number of sessions
	 */
	public int countSessions(Long tenantId, Integer status);

	/**
	 * Retrieves the session of the given SID
	 */
	public Session findBySid(String sid);
	
	/**
	 * Retrieves the session of the given node
	 */
	public List<Session> findByNode(String node);
}