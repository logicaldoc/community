package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;

/**
 * The client side stub for the Google Drive Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@RemoteServiceRelativePath("gdrive")
public interface GDriveService extends RemoteService {

	/**
	 * Uploads a document to Google Drive
	 * 
	 * @param docId ID of the document to upload
	 * 
	 * @return The resourceId of the uploaded document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String upload(long docId) throws ServerException;

	/**
	 * Deletes a document in Google Drive
	 * 
	 * @param resourceId ID of the document to delete
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void delete(String resourceId) throws ServerException;

	/**
	 * Performs the check-in of a Google Drives's document into the LogicalDOC
	 * repository
	 * 
	 * @param docId ID of the document to update
	 * @param comment The comment left for the checkin
	 * @param major If this is a major or minor release
	 * 
	 * @return The checked-in document
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument checkin(long docId, String comment, boolean major) throws ServerException;

	/**
	 * Imports some Google documents into LogicalDOC
	 * 
	 * @param resourceIds IDs of the documents to import
	 * @param targetFolderId ID of the import folder
	 * @param format The type of the documents
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void importDocuments(String[] resourceIds, long targetFolderId, String format) throws ServerException;

	/**
	 * Exports a selection of documents from LogicalDOC into GoogleDocs
	 * 
	 * @param ids The ids of the document to be exported
	 * 
	 * @return The list of the imported documents into Google Drive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] exportDocuments(long[] ids) throws ServerException;

	/**
	 * Save the settings used by the Google Drive module
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
	 * Save the settings used by the Google Drive module
	 * 
	 * @return clientId, clientSecret
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String[] loadSettings() throws ServerException;

	/**
	 * Creates a new Google Document
	 *
	 * @param fileName name of the file to create
	 * 
	 * @return The newly created document's ID in Google Drive
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String create(String fileName) throws ServerException;

	/**
	 * Search in documents into Google Drive
	 * 
	 * @param expression the expression to search
	 *
	 * @return the found hits
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDocument[] search(String expression) throws ServerException;

	public static class Instance {
		private static GDriveServiceAsync instance;

		public static GDriveServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(GDriveService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}