package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.beans.GUIValue;

public interface SystemServiceAsync {

	void getStatistics(String locale, AsyncCallback<GUIParameter[][]> callback);

	void loadTasks(String locale, AsyncCallback<GUITask[]> callback);

	void startTask(String taskName, AsyncCallback<Boolean> callback);

	void stopTask(String taskName, AsyncCallback<Boolean> callback);

	void getTaskByName(String taskName, String locale, AsyncCallback<GUITask> callback);

	void disableTask(String taskName, AsyncCallback<Boolean> callback);

	void enableTask(String taskName, AsyncCallback<Boolean> callback);

	void saveTask(GUITask task, String locale, AsyncCallback<GUITask> callback);

	void setGUILanguageStatus(String language, boolean active, AsyncCallback<Void> callback);

	void getPlugins(AsyncCallback<GUIValue[]> callback);

	void confirmUpdate(AsyncCallback<Void> callback);

	void restart(AsyncCallback<Void> callback);

	void search(Long userId, Date from, Date till, int maxResult, String historySid, String[] event, Long rootFolderId,
			AsyncCallback<GUIHistory[]> callback);
	
	void searchApiCalls(Long userId, Date from, Date till, String callSid, String protocol, String uri, int maxResult,
			AsyncCallback<GUIHistory[]> callback);
}