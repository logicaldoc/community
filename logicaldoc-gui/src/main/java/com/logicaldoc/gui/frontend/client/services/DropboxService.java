package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Dropbox Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
@RemoteServiceRelativePath("dropbox")
public interface DropboxService extends RemoteService {
	/**
	 * Checks if the user has connected the LogicalDOC application to his
	 * Dropbox account
	 * 
	 * @return if the account has been connected
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean isConnected() throws ServerException;

	/**
	 * Starts the authorization process and returns the Dropbox authorization
	 * page URL to be shown to the user
	 * 
	 * @return the authorization token
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String startAuthorization() throws ServerException;

	/**
	 * Ends the authorization code and saves the access token in the database
	 * 
	 * @param authorizationCode the authorization code
	 * 
	 * @return returned value
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String finishAuthorization(String authorizationCode) throws ServerException;

	/**
	 * Exports documents and folders into Dropbox
	 * 
	 * @param targetPath the target path in Dropbox (must be a folder)
	 * @param folderIds Ids of the folders to be imported (all subfolders and
	 *        docs will be imported as well
	 * @param docIds Ids of the documents to be imported
	 * 
	 * @return true, if the export has been successful
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean exportDocuments(String targetPath, List<Long> folderIds, List<Long> docIds) throws ServerException;

	public int importDocuments(long targetFolder, List<String> paths) throws ServerException;

	public static class Instance {
		private static DropboxServiceAsync inst;

		private Instance() {
		}

		public static DropboxServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(DropboxService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}