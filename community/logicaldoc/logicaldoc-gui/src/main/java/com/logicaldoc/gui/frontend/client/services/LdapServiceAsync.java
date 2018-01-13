package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUILdapSettings;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface LdapServiceAsync {

	void loadSettings(AsyncCallback<GUILdapSettings> callback);

	void saveSettings(GUILdapSettings ldapSettings, AsyncCallback<Void> callback);

	void testConnection(GUILdapSettings ldapSettings, AsyncCallback<Boolean> callback);

	void listUsers(String login, AsyncCallback<GUIUser[]> callback);

	void importUsers(String[] usernames, long tenantId, AsyncCallback<GUIValue[]> callback);
}
