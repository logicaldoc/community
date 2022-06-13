package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
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

	void reorderTokenFilters(String[] filters, AsyncCallback<Void> callback);

	void saveTokenFilterSettings(String filter, GUIParameter[] settings, AsyncCallback<Void> callback);

	void setTokenFilterStatus(String language, boolean active, AsyncCallback<Void> callback);
}
