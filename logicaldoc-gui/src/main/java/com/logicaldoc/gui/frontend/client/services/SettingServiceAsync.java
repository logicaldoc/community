package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

public interface SettingServiceAsync {

	void loadProtocolSettings(AsyncCallback<GUIParameter[]> callback);

	void saveSettings(GUIParameter[] settings, AsyncCallback<Void> callback);

	void loadEmailSettings(AsyncCallback<GUIEmailSettings> callback);

	void saveEmailSettings(GUIEmailSettings settings, AsyncCallback<Void> callback);

	void loadGUISettings(AsyncCallback<GUIParameter[]> callback);

	void loadSettingsByNames(List<String> names, AsyncCallback<List<GUIParameter>> callback);

	void loadSettings(AsyncCallback<GUIParameter[]> callback);

	void testEmail(String email, AsyncCallback<Boolean> callback);

	void saveRegistration(String name, String email, String company, String website, AsyncCallback<Void> callback);

	void testStorage(int id, AsyncCallback<Boolean> callback);

	void loadConverterParameters(String converter, AsyncCallback<GUIParameter[]> callback);

	void saveStorageSettings(GUIParameter[] settings, AsyncCallback<Void> callback);

	void saveExtensionAliases(String extension, String aliases, AsyncCallback<Void> callback);

	void removeStorage(int storageId, AsyncCallback<List<String>> callback);

	void loadWebserviceStats(Long tenantId, AsyncCallback<GUIParameter[]> callback);

	void saveFirewallSettings(GUIParameter[] settings, AsyncCallback<Void> callback);
}
