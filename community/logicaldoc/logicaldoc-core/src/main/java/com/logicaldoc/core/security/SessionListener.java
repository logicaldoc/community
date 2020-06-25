package com.logicaldoc.core.security;

/**
 * Events about sessions 
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public interface SessionListener {

	public void onSessionCreated(Session session);
	
	public void onSessionClosed(Object sid);
}
