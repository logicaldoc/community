package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIVIASettings;

public interface VIAServiceAsync {

	void get(AsyncCallback<GUIVIASettings> callback);

	void save(GUIVIASettings settings, AsyncCallback<GUIVIASettings> callback);

}