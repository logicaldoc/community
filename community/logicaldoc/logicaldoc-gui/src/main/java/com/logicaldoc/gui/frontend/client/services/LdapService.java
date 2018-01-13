package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUILdapSettings;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the LdapService.
 */
@RemoteServiceRelativePath("ldap")
public interface LdapService extends RemoteService {
	/**
	 * Saves external authentication settings
	 */
	public void saveSettings(GUILdapSettings ldapSettings) throws ServerException;

	/**
	 * Tests the connection
	 */
	public boolean testConnection(GUILdapSettings ldapSettings) throws ServerException;

	/**
	 * Loads external authentication settings
	 */
	public GUILdapSettings loadSettings() throws ServerException;

	/**
	 * Search for users in the LDAP repository
	 * 
	 * @login used with LIKE operator to restrict the search
	 */
	public GUIUser[] listUsers(String login) throws ServerException;

	/**
	 * Imports a selection of users
	 * 
	 * @param sid The session identifier
	 * @param usernames The list of usernames to import
	 * @param tenantId Tenant the users need to be imported in
	 * 
	 * @return Number of imports, updates, errors.
	 */
	public GUIValue[] importUsers(String[] usernames, long tenantId) throws ServerException;

	public static class Instance {
		private static LdapServiceAsync instance;

		public static LdapServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(LdapService.class);
			}
			return instance;
		}
	}
}