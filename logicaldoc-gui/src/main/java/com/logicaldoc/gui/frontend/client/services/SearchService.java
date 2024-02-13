package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

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
	 * Shares a search among a set of users and groups
	 * 
	 * @param name the name of the search
	 * @param userIds direct ids of users to share the search to
	 * @param groupIds the groups of users to share the search to
	 * 
	 * @throws ServerException share the search to
	 */
	void shareSearch(String name, List<Long> userIds, List<Long> groupIds) throws ServerException;

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
		private static SearchServiceAsync inst;

		private Instance() {
		}

		public static SearchServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SearchService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}