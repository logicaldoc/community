package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUISyndication;

/**
 * The client side stub for the Syndication Service. This service gives all
 * needed methods to handle the syndications.
 */
@RemoteServiceRelativePath("syndication")
public interface SyndicationService extends RemoteService {
	/**
	 * Deletes a given syndication
	 * 
	 * @param id the syndication identifier
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Creates or updates a syndication
	 * 
	 * @param syndication the syndication to save
	 * 
	 * @return the saved syndication
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUISyndication save(GUISyndication syndication) throws ServerException;

	/**
	 * Loads a given syndication from the database
	 * 
	 * @param id identifier of the syndication
	 * 
	 * @return the syndication retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUISyndication getSyndication(long id) throws ServerException;

	/**
	 * Test the connection to the given syndication
	 * 
	 * @param id identifier of the syndication
	 * 
	 * @return if the remote server has been connected
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public boolean test(long id) throws ServerException;

	/**
	 * Changes a syndication enabled/disabled status
	 * 
	 * @param id identifier of the syndication
	 * @param enabled the new status of the syndication
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Cleans the cache
	 * 
	 * @param id identifier of the syndication
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void resetCache(long id) throws ServerException;
	
	public static class Instance {
		private static SyndicationServiceAsync instance;

		public static SyndicationServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SyndicationService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}