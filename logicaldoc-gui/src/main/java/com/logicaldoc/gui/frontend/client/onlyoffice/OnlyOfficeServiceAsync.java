package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface OnlyOfficeServiceAsync {

	void startEditing(long docId, AsyncCallback<Void> callback);

	void endEditing(long docId, AsyncCallback<Void> callback);

	void loadSettings(AsyncCallback<List<GUIValue>> callback);

	void saveSettings(List<GUIValue> settings, AsyncCallback<Void> callback);
}