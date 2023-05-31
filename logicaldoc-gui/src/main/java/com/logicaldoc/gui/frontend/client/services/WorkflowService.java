package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
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
	 * 
	 * @param workflowName name of the workflow
	 * @param version version of the workflow
	 * 
	 * @return the workflow retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIWorkflow get(String workflowName, Integer version) throws ServerException;

	/**
	 * Loads a given workflow to display a completion diagram
	 * 
	 * @param workflowName name of the workflow
	 * @param version version of the workflow template(optional)
	 * @param processInstanceId identifier of the workflow instance
	 * 
	 * @return the completion diagram retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIWorkflow getCompletionDiagram(String workflowName, Integer version, String processInstanceId)
			throws ServerException;

	/**
	 * Deletes a given workflow
	 * 
	 * @param workflowName name of the workflow to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(String workflowName) throws ServerException;

	/**
	 * Deletes a given workflow instance
	 * 
	 * @param id identifier of the workflow instance
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteInstance(String id) throws ServerException;

	/**
	 * Deletes a given workflow instances
	 * 
	 * @param ids identifiers of the workflow instances
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteInstances(String[] ids) throws ServerException;
	
	/**
	 * Imports a new workflow schema already uploaded
	 * 
	 * @return the new workflow representation
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIWorkflow importSchema() throws ServerException;

	/**
	 * Creates or updates a workflow
	 * 
	 * @param workflow the workflow to save
	 * 
	 * @return the saved workflow
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIWorkflow save(GUIWorkflow workflow) throws ServerException;

	/**
	 * Deploys a given workflow
	 * 
	 * @param workflow the workflow to deploy
	 * 
	 * @return the next version of the workflow
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIWorkflow deploy(GUIWorkflow workflow) throws ServerException;

	/**
	 * Undeploys a given workflow
	 * 
	 * @param workflowName name of the workflow
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void undeploy(String workflowName) throws ServerException;

	/**
	 * Lists all the workflows on the database
	 * 
	 * @return all the available workflows
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public GUIWorkflow[] list() throws ServerException;

	/**
	 * Deletes a workflow trigger
	 * 
	 * @param id identifier of the trigger
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void deleteTrigger(long id) throws ServerException;

	/**
	 * Applies the triggers on a root folder to all the subtree
	 * 
	 * @param folderId identifier of the folder root of the tree
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void applyTriggersToTree(long folderId) throws ServerException;

	/**
	 * Save a new workflow trigger on the given folder with the given workflowId
	 * and templateId
	 * 
	 * @param folderId identifier of the folder
	 * @param workflowId identifier of the workflow
	 * @param templateId identifier of the template(optional)
	 * @param events the comma separated list of events(null for all the events)
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void saveTrigger(String folderId, String workflowId, String templateId, String events)
			throws ServerException;

	/**
	 * Start a workflow with the given name and associated to the documents with
	 * the given doc ids
	 * 
	 * @param workflowName name of the workflow
	 * @param workflowDescription description of the workflow
	 * @param tag a tak to mark this new execution
	 * @param docIds identifiers of the documents appended to the new workflow
	 *        instance
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void startWorkflow(String workflowName, String workflowDescription, String tag, long[] docIds)
			throws ServerException;

	/**
	 * Retrieves all the info of the workflow of the given task
	 * 
	 * @param taskId identifier of the task
	 * 
	 * @return the workflow retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public GUIWorkflow getWorkflowDetailsByTask(String taskId) throws ServerException;

	/**
	 * The given user take the ownership of the task. If the task is already
	 * claimed you cannot claim again
	 * 
	 * @param taskId identifier of the task
	 * @param userId identifier of the user
	 * 
	 * @return the workflow definition
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public GUIWorkflow claimTask(String taskId, String userId) throws ServerException;

	/**
	 * The task is assigned to another user
	 * 
	 * @param taskId identifier of the task
	 * @param userId identifier of the user
	 * 
	 * @return the workflow definition
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public GUIWorkflow reassignTask(String taskId, String userId) throws ServerException;

	/**
	 * The task is reassigned to the pooled users
	 * 
	 * @param taskId identifier of the task
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void turnBackTaskToPool(String taskId) throws ServerException;

	/**
	 * Ends a task invoking the transition
	 * 
	 * @param taskId identifier of the task
	 * @param transitionName name of the transition
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void endTask(String taskId, String transitionName) throws ServerException;

	/**
	 * Counts all the tasks assigned to the given user
	 * 
	 * @param username the username
	 * 
	 * @return number of tasks assigned to <code>username</code>
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public int countAssignedTasks(String username) throws ServerException;

	/**
	 * Appends to the workflow of the given taskId the documents with the given
	 * doc ids
	 * 
	 * @param taskId identifier of the task
	 * @param docIds identifiers of the documents to append
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void appendDocuments(String taskId, Long[] docIds) throws ServerException;

	/**
	 * Detaches a document from a workflow
	 * 
	 * @param taskId identifier of the task
	 * @param docId identifier of the document to remove
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void removeDocument(String taskId, long docId) throws ServerException;

	/**
	 * Adds a new note on the workflow instance
	 * 
	 * @param taskId identifier of the task
	 * @param note the new note
	 * 
	 * @return identifier of the new note
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public long addNote(String taskId, String note) throws ServerException;

	/**
	 * Deletes a note
	 * 
	 * @param noteId identifier of the note
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void deleteNote(long noteId) throws ServerException;

	public static class Instance {
		private static WorkflowServiceAsync inst;

		private Instance() {
		}
		
		public static WorkflowServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(WorkflowService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}