package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * The client side stub for the Zoho Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.2
 */
@RemoteServiceRelativePath("zoho")
public interface ZohoService extends RemoteService {

	/**
	 * Save the settings used by the Zoho module
	 *
	 * @param clientId identifier of the client
	 * @param clientSecret the secret key specified by he user
	 * 
	 * @return The URL of the consent page
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String saveSettings(String clientId, String clientSecret) throws ServerException;

	/**
	 * Save the settings used by the Zoho module
	 * 
	 * @return clientId, clientSecret
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] loadSettings() throws ServerException;

	/**
	 * Exports documents and folders into Zoho
	 * 
	 * @param targetFolderId the target path in Zoho (must be a folder)
	 * @param folderIds Ids of the folders to be imported (all subfolders and
	 *        docs will be imported as well
	 * @param docIds Ids of the documents to be imported
	 * 
	 * @return true if the import was successful
	 * 
	 * @throws ServerException error in the server application
	 */
	public boolean exportDocuments(String targetFolderId, long[] folderIds, long[] docIds) throws ServerException;

	/**
	 * Exports documents and folders from Zoho into LogicalDOC
	 * 
	 * @param targetFolder ID of the root folder that will receive the imported
	 *        elements
	 * @param folderCompositeIds array of the identifiers of the Zoho folder
	 *        each one is a tokenized string folder_name:folder_id
	 * @param documentIds identifiers of the documents in Zoho to be imported
	 * 
	 * @return number of imported files
	 * 
	 * @throws ServerException error in the server application
	 */
	public int importDocuments(long targetFolder, String[] folderCompositeIds, String[] documentIds)
			throws ServerException;

	/**
	 * Uploads a document to Zoho.
	 * 
	 * @param docId ID of the document to upload
	 * 
	 * @return The resourceId of the uploaded document
	 * 
	 * @throws ServerException error in the server application
	 */
	public String upload(long docId) throws ServerException;

	/**
	 * Deletes a document in Zoho
	 * 
	 * @param resourceId identifier of the document to delete
	 * 
	 * @throws ServerException error in the server application
	 */
	public void delete(String resourceId) throws ServerException;

	/**
	 * Performs the check-in of a Zoho's document into the LogicalDOC
	 * repository
	 * 
	 * @param docId identifier of the document to update
	 * @param comment The comment left for the checkin
	 * @param major If this is a major or minor release
	 * 
	 * @return The checked-in document
	 * 
	 * @throws ServerException error in the server application
	 */
	public GUIDocument checkin(long docId, String comment, boolean major) throws ServerException;

	public static class Instance {
		private static ZohoServiceAsync instance;

		private Instance() {
		}
		
		public static ZohoServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ZohoService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}