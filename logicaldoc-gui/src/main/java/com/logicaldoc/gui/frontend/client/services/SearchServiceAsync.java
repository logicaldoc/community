package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

public interface SearchServiceAsync {

	void search(GUISearchOptions options, AsyncCallback<GUIResult> callback);

	void save(GUISearchOptions options, AsyncCallback<Void> callback);

	void load(String name, AsyncCallback<GUISearchOptions> callback);

	void delete(List<String> names, AsyncCallback<Void> callback);

	void shareSearch(String name, List<Long> userIds, List<Long> groupIds, AsyncCallback<Void> callback);
}
