package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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

	public void saveAuthToken(String authToken) throws ServerException;

	public String loadAuthToken() throws ServerException;

	/**
	 * Exports documents and folders into Zoho
	 * 
	 * @param sid The session ID
	 * @param targetPath the target path in Zoho (must be a folder)
	 * @param folderIds Ids of the folders to be imported (all subfolders and
	 *        docs will be imported as well
	 * @param docIds Ids of the documents to be imported
	 * @return
	 * @throws ServerException
	 */
	public boolean exportDocuments(String targetFolderId, long[] folderIds, long[] docIds) throws ServerException;

	/**
	 * Exports documents and folders from Zoho into LogicalDOC
	 * 
	 * @param sid The session ID
	 * @param targetFolder ID of the root folder that will receive the imported
	 *        elements
	 * @param folderCompositeIds array of the identifiers of the Zoho folder
	 *        each one is a tokenized string folder_name:folder_id
	 * @param docIds Ids of the documents in Zoho to be imported
	 * @return
	 * @throws ServerException
	 */
	public int importDocuments(long targetFolder, String[] folderCompositeIds, String[] documentIds)
			throws ServerException;

	/**
	 * Uploads a document to Zoho.
	 * 
	 * @param docId ID of the document to upload
	 * 
	 * @returns The resourceId of the uploaded document
	 */
	public String upload(long docId) throws ServerException;

	/**
	 * Deletes a document in Zoho.
	 * 
	 * @param docId ID of the document to delete
	 */
	public void delete(String resourceId) throws ServerException;

	/**
	 * Performs the check-in of a Zoho's document into the LogicalDOC
	 * repository.
	 * 
	 * @param docId ID of the document to update
	 * @param comment The comment left for the checkin
	 * @param major If this is a major or minor release
	 * 
	 * @return The checked-in document
	 */
	public GUIDocument checkin(long docId, String comment, boolean major) throws ServerException;

	public static class Instance {
		private static ZohoServiceAsync instance;

		public static ZohoServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ZohoService.class);
			}
			return instance;
		}
	}
}