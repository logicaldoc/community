package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;

/**
 * The client side stub for the ImportFolder Service. This service gives all
 * needed methods to handle import folders.
 */
@RemoteServiceRelativePath("importfolder")
public interface ImportFolderService extends RemoteService {
	/**
	 * Deletes a given import folder
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates an import folder
	 * 
	 * @param importFolder the import folder to save
	 * 
	 * @return the saved import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIImportFolder save(GUIImportFolder importFolder) throws ServerException;

	/**
	 * Loads a given import folder from the database
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @return the import folder retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIImportFolder get(long id) throws ServerException;

	/**
	 * Tests the connection to the given import folder
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @return if the import folder has been connected and able to work
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean test(long id) throws ServerException;

	/**
	 * Changes a importFolder enabled/disabled status
	 * 
	 * @param id identifier of the import folder
	 * @param enabled flag to enable / disable the import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Reset the import counter
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetCounter(long id) throws ServerException;

	/**
	 * Cleans the cache
	 * 
	 * @param id identifier of the import folder
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetCache(long id) throws ServerException;

	/**
	 * Clones a given import folder
	 * 
	 * @param id Identifier of the original import folder to clone
	 * 
	 * @return The created clone
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIImportFolder clone(long id) throws ServerException;

	public static class Instance {
		private static ImportFolderServiceAsync inst;

		private Instance() {
		}

		public static ImportFolderServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ImportFolderService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}