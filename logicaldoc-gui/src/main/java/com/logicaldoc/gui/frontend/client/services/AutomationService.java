package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;

/**
 * The client side stub for the Automation Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
@RemoteServiceRelativePath("automation")
public interface AutomationService extends RemoteService {
	/**
	 * Creates or updates a routine
	 * 
	 * @param routine the routine to save
	 * 
	 * @return the saved routine
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAutomationRoutine saveRoutine(GUIAutomationRoutine routine) throws ServerException;

	/**
	 * Loads a given routine from the database
	 * 
	 * @param id identifier of the routine
	 * 
	 * @return the routine retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAutomationRoutine getRoutine(long id) throws ServerException;

	public void deleteRoutines(List<Long> ids) throws ServerException;

	public void deleteTriggers(List<Long> ids) throws ServerException;

	/**
	 * Creates or updates a trigger
	 * 
	 * @param trigger the automation trigger to save
	 * 
	 * @return the saved trigger
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAutomationTrigger saveTrigger(GUIAutomationTrigger trigger) throws ServerException;

	/**
	 * Loads a given trigger from the database
	 * 
	 * @param id identifier of the trigger
	 * 
	 * @return the automation trigger retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIAutomationTrigger getTrigger(long id) throws ServerException;

	/**
	 * Applies the triggers on a root folder to all the subtree
	 * 
	 * @param rootId identifier of the root folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyTriggersToTree(long rootId) throws ServerException;

	/**
	 * Executes a routine or a given script
	 * 
	 * @param routine The automation routine to execute, if the
	 *        <code>routine.id=0</code> then the <code>routine.script</code> is
	 *        executed, otherwise the referenced routine is picked from the
	 *        database and the extended attributes of <code>routine</code> are
	 *        used as input parameters.
	 * @param docIds selected documents (optional)
	 * @param folderId selected folders (optional)
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void execute(GUIAutomationRoutine routine, List<Long> docIds, Long folderId) throws ServerException;

	public static class Instance {
		private static AutomationServiceAsync inst;

		private Instance() {
		}

		public static AutomationServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(AutomationService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}