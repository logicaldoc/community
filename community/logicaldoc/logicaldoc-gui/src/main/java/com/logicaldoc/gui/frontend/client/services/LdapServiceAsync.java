package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface LDAPServiceAsync {
	
	void save(GUILDAPServer ldapServer, AsyncCallback<GUILDAPServer> callback);

	void testConnection(GUILDAPServer ldapSettings, AsyncCallback<Boolean> callback);

	void listUsers(String login, long serverId, AsyncCallback<GUIUser[]> callback);

	void importUsers(String[] usernames, long tenantId, AsyncCallback<GUIValue[]> callback);

	void get(long serverId, AsyncCallback<GUILDAPServer> callback);

	void delete(long serverId, AsyncCallback<Void> callback);

	void reorder(Long[] serverIds, AsyncCallback<Void> callback);
}
