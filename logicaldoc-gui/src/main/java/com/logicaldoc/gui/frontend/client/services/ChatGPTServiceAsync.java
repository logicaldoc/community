package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface ChatGPTServiceAsync {

	void loadSettings(AsyncCallback<List<GUIValue>> callback);

	void saveSettings(List<GUIValue> settings, AsyncCallback<Void> callback);

	void startThread(String question, List<GUIDocument> documents, AsyncCallback<Void> callback);

	void ask(String question, AsyncCallback<Void> callback);

	void getAnswer(AsyncCallback<GUIValue> callback);
}