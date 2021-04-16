package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

/**
 * Service responsible of Searches
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("search")
public interface SearchService extends RemoteService {

	/**
	 * Performs a search against the database
	 * 
	 * @param options The search options
	 * 
	 * @return Result hits and statistics
	 * 
	 * @throws ServerException error in the server application
	 */
	public GUIResult search(GUISearchOptions options) throws ServerException;

	/**
	 * Saves the search options in the user's working dir
	 * 
	 * @param options The search options
	 * 
	 * @return true if the operation was successful and there were no duplicates
	 * 
	 * @throws ServerException error in the server application
	 */
	public boolean save(GUISearchOptions options) throws ServerException;

	/**
	 * Re-create a saved search in other user profiles
	 * 
	 * @param name name of the search to replicate
	 * @param userIds identifiers of the users
	 * 
	 * @throws ServerException error in the server application
	 */
	public void shareSearch(String name, long[] userIds) throws ServerException;

	/**
	 * Deletes a previously saved search
	 * 
	 * @param names The saved search names
	 * 
	 * @throws ServerException error in the server application
	 */
	public void delete(String[] names) throws ServerException;

	/**
	 * Loads a saved search
	 * 
	 * @param name The saved search name
	 * 
	 * @return the options
	 * 
	 * @throws ServerException error in the server application
	 */
	public GUISearchOptions load(String name) throws ServerException;

	public static class Instance {
		private static SearchServiceAsync instance;

		public static SearchServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SearchService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}