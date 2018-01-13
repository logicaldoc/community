package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;

/**
 * The client side stub for the Workflow Service. This service gives all needed
 * methods to handle workflows.
 */
@RemoteServiceRelativePath("workflow")
public interface WorkflowService extends RemoteService {
	/**
	 * Loads a given workflow from the database
	 */
	public GUIWorkflow get(String workflowName) throws ServerException;

	/**
	 * Deletes a given workflow
	 */
	public void delete(String workflowName) throws ServerException;

	/**
	 * Deletes a given workflow instance
	 */
	public void deleteInstance(String id) throws ServerException;

	/**
	 * Imports a new workflow schema.
	 */
	public GUIWorkflow importSchema() throws ServerException;

	/**
	 * Creates or updates a workflow
	 */
	public GUIWorkflow save(GUIWorkflow workflow) throws ServerException;

	/**
	 * Deploys a given workflow
	 */
	public void deploy(GUIWorkflow workflow) throws ServerException;

	/**
	 * Undeploys a given workflow
	 */
	public void undeploy(String workflowName) throws ServerException;

	/**
	 * Lists all the workflows on the database
	 */
	public GUIWorkflow[] list() throws ServerException;

	/**
	 * Deletes a workflow trigger
	 */
	public void deleteTrigger(long id) throws ServerException;

	/**
	 * Applies the triggers on a root folder to all the subtree
	 */
	public void applyTriggersToTree(long rootId) throws ServerException;

	/**
	 * Save a new workflow trigger on the given folder with the given workflowId
	 * and templateId.
	 */
	public void saveTrigger(String folderId, String workflowId, String templateId, int startAtCheckin)
			throws ServerException;

	/**
	 * Start a workflow with the given name and associated to the documents with
	 * the given doc ids.
	 */
	public void startWorkflow(String workflowName, String workflowDescription, String tag, long[] docIds)
			throws ServerException;

	/**
	 * Retrieves all the info of the workflow of the given task.
	 */
	public GUIWorkflow getWorkflowDetailsByTask(String taskId) throws ServerException;

	/**
	 * The given user take the ownership of the task. If the task is already
	 * claimed you cannot claim again.
	 */
	public GUIWorkflow claimTask(String taskId, String userId) throws ServerException;

	/**
	 * The task is assigned to another user
	 */
	public GUIWorkflow reassignTask(String taskId, String userId) throws ServerException;

	/**
	 * The task is reassigned to the pooled users.
	 */
	public void turnBackTaskToPool(String taskId) throws ServerException;

	/**
	 * Ends a task invoking the transition.
	 */
	public void endTask(String taskId, String transitionName) throws ServerException;

	/**
	 * Counts all the tasks assigned to the given user.
	 */
	public int countActiveUserTasks(String username) throws ServerException;

	/**
	 * Appends to the workflow of the given taskId the documents with the given
	 * doc ids.
	 */
	public void appendDocuments(String taskId, Long[] docIds) throws ServerException;

	/**
	 * Detaches a document from a workflow
	 */
	public void removeDocument(String taskId, long docId) throws ServerException;

	/**
	 * Adds a new note
	 */
	public long addNote(String taskId, String note) throws ServerException;

	/**
	 * Deletes a note
	 */
	public void deleteNote(long noteId) throws ServerException;

	public static class Instance {
		private static WorkflowServiceAsync instance;

		public static WorkflowServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(WorkflowService.class);
			}
			return instance;
		}
	}
}