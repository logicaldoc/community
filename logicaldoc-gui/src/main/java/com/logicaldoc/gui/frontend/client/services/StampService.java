package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIStamp;

/**
 * The client side stub for the Stamp Service. This service gives all needed
 * methods to handle the stamps.
 */
@RemoteServiceRelativePath("stamp")
public interface StampService extends RemoteService {
	/**
	 * Deletes a given stamp
	 * 
	 * @param id identifier of the stamp to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates a stamp
	 * 
	 * @param stamp the stamp to save
	 * 
	 * @return the saved stamp
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIStamp save(GUIStamp stamp) throws ServerException;

	/**
	 * Saves the stamp's image already uploaded
	 * 
	 * @param stampId identifier of the stamp
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveImage(long stampId) throws ServerException;

	/**
	 * Loads the stamp that represents the signature of the customer. If it does
	 * not exist a default one is created and returned.
	 * 
	 * @return the stamp retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIStamp getSignature() throws ServerException;

	/**
	 * Loads a given stamp from the database
	 * 
	 * @param name unique name of the stamp
	 * 
	 * @return the stamp retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIStamp getStamp(String name) throws ServerException;

	/**
	 * Loads a given stamp from the database
	 * 
	 * @param stampId identifier of the stamp
	 * 
	 * @return the stamp retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIStamp getStamp(long stampId) throws ServerException;

	/**
	 * Changes a stamp enabled/disabled status
	 * 
	 * @param stampId identifier of the stamp
	 * @param enabled the enabled status of the stamp
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long stampId, boolean enabled) throws ServerException;

	/**
	 * Applies a stamp to the given document
	 * 
	 * @param docIds identifiers of the documents
	 * @param stamp the stamp to apply
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void applyStamp(long[] docIds, GUIStamp stamp) throws ServerException;

	/**
	 * Remove users from stamp
	 * 
	 * @param userIds identifiers of the users to remove from the stamp
	 * @param stampId identifier of the stamp
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void removeUsers(long[] userIds, long stampId) throws ServerException;

	/**
	 * Assigns users to stamp
	 * 
	 * @param userIds identifiers of the users to associate to the stamp
	 * @param stampId identifier of the stamp
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void addUsers(long[] userIds, long stampId) throws ServerException;

	public static class Instance {
		private static StampServiceAsync instance;

		public static StampServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(StampService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}