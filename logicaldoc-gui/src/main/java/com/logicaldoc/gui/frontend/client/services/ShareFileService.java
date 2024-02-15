package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

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
	public boolean exportDocuments(String targetFolder, List<Long> folderIds, List<Long> docIds) throws ServerException;

	public int importDocuments(long targetFolder, List<String> itemIds) throws ServerException;

	/**
	 * Retrieve the settings saved for connecting to FileShare
	 * 
	 * @return the settings
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<String> loadSettings() throws ServerException;

	/**
	 * Saves the settings into the database and returns the authorization URL
	 * the user must be redirected to.
	 * 
	 * @param clientId the client identifier
	 * @param clientSecret the client secret
	 * 
	 * @return the authorization URL
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String authorize(String clientId, String clientSecret) throws ServerException;

	/**
	 * Checks if the current user is authorized to interact with ShreFile
	 * 
	 * @return if you are authorized
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean isAuthorized() throws ServerException;

	public static class Instance {
		private static ShareFileServiceAsync inst;

		private Instance() {
		}

		public static ShareFileServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ShareFileService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}