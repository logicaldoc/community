package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;

public interface AutomationServiceAsync {

	void deleteRoutines(long[] ids, AsyncCallback<Void> callback);

	void deleteTriggers(long[] ids, AsyncCallback<Void> callback);

	void saveTrigger(GUIAutomationTrigger trigger, AsyncCallback<GUIAutomationTrigger> callback);

	void getTrigger(long id, AsyncCallback<GUIAutomationTrigger> callback);

	void getRoutine(long id, AsyncCallback<GUIAutomationRoutine> callback);

	void saveRoutine(GUIAutomationRoutine routine, AsyncCallback<GUIAutomationRoutine> callback);

	void applyTriggersToTree(long rootId, AsyncCallback<Void> callback);

	void execute(GUIAutomationRoutine routine, long[] docIds, Long folderId, AsyncCallback<Void> callback);
}