package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;

public interface SearchEngineServiceAsync {

	void getInfo(AsyncCallback<GUISearchEngine> callback);

	void rescheduleAll(boolean dropIndex, AsyncCallback<Void> callback);

	void unlock(AsyncCallback<Void> callback);

	void save(GUISearchEngine searchEngine, AsyncCallback<Void> callback);

	void setLanguageStatus(String language, boolean active, AsyncCallback<Void> callback);

	void check(AsyncCallback<String> callback);

	void setAliases(String extension, String aliases, AsyncCallback<Void> callback);

	void countEntries(AsyncCallback<Long> callback);

	void reorderTokenFilters(List<String> filters, AsyncCallback<Void> callback);

	void saveTokenFilterSettings(String filter, GUIParameter[] settings, AsyncCallback<Void> callback);

	void setTokenFilterStatus(String language, boolean active, AsyncCallback<Void> callback);

	void purge(AsyncCallback<Void> callback);

	void query(String query, int page, int size, AsyncCallback<GUIResult> callback);

	void remove(List<Long> entryIds, AsyncCallback<Void> callback);
}
