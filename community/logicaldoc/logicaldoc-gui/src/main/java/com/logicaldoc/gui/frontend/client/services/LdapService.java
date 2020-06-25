package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the LdapService.
 */
@RemoteServiceRelativePath("ldap")
public interface LDAPService extends RemoteService {
	/**
	 * Saves external authentication settings
	 * 
	 * @param ldapServer the external authentication server
	 * 
	 * @return the saved Saves external authentication settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUILDAPServer save(GUILDAPServer ldapServer) throws ServerException;

	/**
	 * Tests the connection
	 * 
	 * @param ldapServer the external authentication server
	 * 
	 * @return true if the external authentication server allowed the access
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean testConnection(GUILDAPServer ldapServer) throws ServerException;

	/**
	 * Loads external authentication settings
	 * 
	 * @param serverId identifier of the external authentication server 
	 * 
	 * @return the external authentication settings retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUILDAPServer get(long serverId) throws ServerException;

	/**
	 * Deletes the specified server
	 * 
	 * @param serverId identifier of the authentication server
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long serverId) throws ServerException;

	public void reorder(Long[] serverIds) throws ServerException;
	
	/**
	 * Search for users in the LDAP repository
	 * 
	 * @param login used with LIKE operator to restrict the search
	 * @param serverId identifier of the LDAP server to use
	 * 
	 * @return the users that match the login
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIUser[] listUsers(String login, long serverId) throws ServerException;

	/**
	 * Imports a selection of users
	 * 
	 * @param usernames the list of usernames to import
	 * @param serverId identifier of the server to use
	 * 
	 * @return number of imports, updates, errors
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIValue[] importUsers(String[] usernames, long serverId) throws ServerException;

	public static class Instance {
		private static LDAPServiceAsync instance;

		public static LDAPServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(LDAPService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}