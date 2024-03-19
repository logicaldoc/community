package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;

public interface WorkflowServiceAsync {

	void delete(String name, AsyncCallback<Void> callback);

	void get(String workflowName, Integer version, AsyncCallback<GUIWorkflow> callback);

	void deploy(GUIWorkflow workflow, AsyncCallback<GUIWorkflow> callback);

	void list(AsyncCallback<List<GUIWorkflow>> callback);

	void save(GUIWorkflow workflow, AsyncCallback<GUIWorkflow> callback);

	void saveACL(GUIWorkflow workflow, AsyncCallback<Void> callback);

	void deleteTrigger(long id, AsyncCallback<Void> callback);

	void saveTrigger(String folderId, String workflowId, String templateId, String events,
			AsyncCallback<Void> callback);

	void startWorkflow(String workflowName, String workflowDescription, String tag, String color, List<Long> docIds,
			AsyncCallback<Void> callback);

	void getWorkflowDetailsByTask(String taskId, AsyncCallback<GUIWorkflow> callback);

	void endTask(String taskId, String transitionName, AsyncCallback<Void> callback);

	void claimTask(String taskId, String userId, AsyncCallback<GUIWorkflow> callback);

	void turnBackTaskToPool(String taskId, AsyncCallback<Void> callback);

	void countAssignedTasks(String username, AsyncCallback<Integer> callback);

	void appendDocuments(String taskId, List<Long> docIds, AsyncCallback<Void> callback);

	void importSchema(AsyncCallback<GUIWorkflow> callback);

	void applyTriggersToTree(long rootId, AsyncCallback<Void> callback);

	void deleteInstance(String id, AsyncCallback<Void> callback);

	void reassignTask(String taskId, String userId, AsyncCallback<GUIWorkflow> callback);

	void undeploy(String workflowName, AsyncCallback<Void> callback);

	void addNote(String taskId, String transitionName, String note, AsyncCallback<Long> callback);

	void deleteNote(long noteId, AsyncCallback<Void> callback);

	void removeDocument(String instanceId, long docId, AsyncCallback<Void> callback);

	void getCompletionDiagram(String workflowName, Integer version, String processInstanceId,
			AsyncCallback<GUIWorkflow> callback);

	void deleteInstances(List<String> ids, AsyncCallback<Void> callback);

}
