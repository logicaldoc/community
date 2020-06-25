package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIHistory;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITask;
import com.logicaldoc.gui.common.client.beans.GUIValue;

/**
 * The client side stub for the System Service. This service allows the
 * management of various system settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("system")
public interface SystemService extends RemoteService {

	/**
	 * Retrieves all the statistics parameters.
	 * 
	 * <ol>
	 * <li>The first array is the Repository statistics.</li>
	 * <li>The second array is the Documents statistics.</li>
	 * <li>The third array is the Folders statistics.</li>
	 * <li>The fourth array contains the last run date.</li>
	 * </ol>
	 * 
	 * @param locale The current user locale
	 * 
	 * @return the statistics
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIParameter[][] getStatistics(String locale) throws ServerException;

	public GUIHistory[] search(String userName, Date from, Date till, int maxResult, String historySid, String[] event,
			Long rootFolderId) throws ServerException;

	/**
	 * Retrieves all tasks.
	 * 
	 * @param locale The current user locale
	 * 
	 * @return the tasks
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITask[] loadTasks(String locale) throws ServerException;

	/**
	 * Starts the task execution.
	 * 
	 * @param taskName The task name
	 * 
	 * @return True, if the task is correctly started.
	 */
	public boolean startTask(String taskName);

	/**
	 * Stops the task execution.
	 * 
	 * @param taskName The task name
	 * 
	 * @return True, if the task is correctly stopped.
	 */
	public boolean stopTask(String taskName);

	/**
	 * Retrieves a specific task by its name
	 * 
	 * @param taskName The task name
	 * @param locale The current user locale
	 * 
	 * @return the task retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITask getTaskByName(String taskName, String locale) throws ServerException;

	/**
	 * Enables the task
	 * 
	 * @param taskName The task name
	 * 
	 * @return True, if the task is correctly enabled
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public boolean enableTask(String taskName) throws ServerException;

	/**
	 * Disables the task
	 * 
	 * @param taskName The task name
	 * 
	 * @return True, if the task is correctly disabled
	 * 
	 * @throws ServerException error in the server application
	 */
	public boolean disableTask(String taskName) throws ServerException;

	/**
	 * Saves the task
	 * 
	 * @param task The task to save
	 * @param locale The current user locale
	 * 
	 * @return the saved task
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUITask saveTask(GUITask task, String locale) throws ServerException;

	/**
	 * Changes the activation status of a language
	 * 
	 * @param language the language to alter
	 * @param active the new language's status
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setGUILanguageStatus(String language, boolean active) throws ServerException;

	/**
	 * Retrieves all plug-ins
	 * 
	 * @return the installed plug-ins names
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIValue[] getPlugins() throws ServerException;

	/**
	 * Confirms the last update
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void confirmUpdate() throws ServerException;

	/**
	 * Restarts the application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void restart() throws ServerException;

	public static class Instance {
		private static SystemServiceAsync instance;

		public static SystemServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SystemService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}