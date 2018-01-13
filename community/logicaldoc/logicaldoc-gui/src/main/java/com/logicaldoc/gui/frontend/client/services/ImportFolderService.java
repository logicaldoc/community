package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIImportFolder;

/**
 * The client side stub for the ImportFolder Service. This service gives all
 * needed methods to handle import folders.
 */
@RemoteServiceRelativePath("importfolder")
public interface ImportFolderService extends RemoteService {
	/**
	 * Deletes a given folder
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates an import folder
	 */
	public GUIImportFolder save(GUIImportFolder share) throws ServerException;

	/**
	 * Loads a given import folder from the database
	 */
	public GUIImportFolder getImportFolder(long id) throws ServerException;

	/**
	 * Test the connection to the given import folder
	 */
	public boolean test(long id) throws ServerException;

	/**
	 * Changes a importFolder enabled/disabled status
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Cleans the cache
	 */
	public void resetCache(long id) throws ServerException;

	public static class Instance {
		private static ImportFolderServiceAsync instance;

		public static ImportFolderServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ImportFolderService.class);
			}
			return instance;
		}
	}
}