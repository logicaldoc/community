package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Dropbox Service.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.0
 */
@RemoteServiceRelativePath("dropbox")
public interface DropboxService extends RemoteService {
	/**
	 * Checks if the user has connected the LogicalDOC application to his
	 * Dropbox account.
	 */
	public boolean isConnected() throws ServerException;

	/**
	 * Starts the authorization process and returns the Dropbox authorization
	 * page URL to be shown to the user.
	 */
	public String startAuthorization() throws ServerException;

	/**
	 * Ends the authorization code and saves the access token in the database.
	 */
	public String finishAuthorization(String authorizationCode) throws ServerException;

	/**
	 * Exports documents and folders into Dropbox
	 * 
	 * @param sid The session ID
	 * @param targetPath the target path in Dropbox (must be a folder)
	 * @param folderIds Ids of the folders to be imported (all subfolders and
	 *        docs will be imported as well
	 * @param docIds Ids of the documents to be imported
	 * @return
	 * @throws ServerException
	 */
	public boolean exportDocuments(String targetPath, long[] folderIds, long[] docIds)
			throws ServerException;
	
	public int importDocuments(long targetFolder, String[] paths)
			throws ServerException;
	
	public static class Instance {
		private static DropboxServiceAsync instance;

		public static DropboxServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(DropboxService.class);
			}
			return instance;
		}
	}
}