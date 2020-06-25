package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the ShareFile Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
@RemoteServiceRelativePath("sharefile")
public interface ShareFileService extends RemoteService {
	/**
	 * Exports documents and folders into ShareFille
	 * 
	 * @param targetFolder the target folder in ShareFile
	 * @param folderIds Ids of the folders to be imported (all sub-folders and
	 *        docs will be imported as well)
	 * @param docIds Ids of the documents to be imported
	 * 
	 * @return if the document have been exported successfully
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean exportDocuments(String targetFolder, long[] folderIds, long[] docIds) throws ServerException;

	public int importDocuments(long targetFolder, String[] itemIds) throws ServerException;

	/**
	 * Save the settings used by the FileShare module
	 * 
	 * @param hostname name of the host
	 * @param username username
	 * @param password password
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveSettings(String hostname, String username, String password) throws ServerException;

	/**
	 * Retrieve the settings saved for connecting to FileShare
	 * 
	 * @return the settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] loadSettings() throws ServerException;

	public static class Instance {
		private static ShareFileServiceAsync instance;

		public static ShareFileServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ShareFileService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}