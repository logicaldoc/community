package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates a stamp
	 */
	public GUIStamp save(GUIStamp stamp) throws ServerException;

	/**
	 * Saves the stamp's image
	 */
	public void saveImage(long stampId) throws ServerException;

	/**
	 * Loads a given stamp from the database
	 */
	public GUIStamp getStamp(long id) throws ServerException;

	/**
	 * Changes a stamp enabled/disabled status
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Applies a stamp to the given document
	 */
	public void applyStamp(long[] docIds, long stampId) throws ServerException;

	/**
	 * Remove users from stamp
	 */
	public void removeUsers(long[] userIds, long stampId) throws ServerException;

	/**
	 * Assigns users to stamp
	 */
	public void addUsers(long[] userIds, long stampId) throws ServerException;

	public static class Instance {
		private static StampServiceAsync instance;

		public static StampServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(StampService.class);
			}
			return instance;
		}
	}
}