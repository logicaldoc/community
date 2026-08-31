package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface SystemServiceAsync {

	void getStatistics(String locale, AsyncCallback<List<List<GUIParameter>>> callback);

	void loadTasks(String locale, AsyncCallback<List<GUITask>> callback);

	void startTask(String taskName, AsyncCallback<Void> callback);

	void stopTask(String taskName, AsyncCallback<Void> callback);

	void getTaskByName(String taskName, String locale, AsyncCallback<GUITask> callback);

	void disableTask(String taskName, AsyncCallback<Boolean> callback);

	void enableTask(String taskName, AsyncCallback<Boolean> callback);

	void saveTask(GUITask task, String locale, AsyncCallback<GUITask> callback);

	void setGUILanguageStatus(String language, boolean active, AsyncCallback<Void> callback);

	void getPlugins(AsyncCallback<List<GUIValue>> callback);

	void confirmUpdate(AsyncCallback<Void> callback);

	void restart(AsyncCallback<Void> callback);

	void search(Long userId, Date from, Date till, int maxResult, String historySid, List<String> event,
			Long rootFolderId, AsyncCallback<List<GUIHistory>> callback);

	void searchApiCalls(Long userId, Date from, Date till, String callSid, String protocol, String uri, int maxResult,
			AsyncCallback<List<GUIHistory>> callback);

	void unscheduleJobs(List<GUIValue> jobs, AsyncCallback<Void> callback);

	void initializePlugin(String plugin, AsyncCallback<Void> callback);

	void installPlugin(AsyncCallback<Void> callback);

	void uninstallPlugin(String plugin, AsyncCallback<Void> callback);

	void saveLogger(String name, String level, boolean additivity, AsyncCallback<Void> callback);

	void removeLogger(String name, AsyncCallback<Void> callback);
}