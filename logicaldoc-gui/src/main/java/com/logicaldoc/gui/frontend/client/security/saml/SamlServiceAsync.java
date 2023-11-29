package com.logicaldoc.gui.frontend.client.security.saml;

import com.google.gwt.user.client.rpc.AsyncCallback;

public interface SamlServiceAsync {

	void loadSettings(AsyncCallback<GUISamlSettings> callback);

	public void saveSettings(GUISamlSettings settings, AsyncCallback<Void> callback);

	public void importResource(String resourceName, AsyncCallback<String> callback);
}