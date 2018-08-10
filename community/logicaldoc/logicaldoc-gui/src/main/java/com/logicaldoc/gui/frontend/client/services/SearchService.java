package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 * @param sid The current user session
	 * @param options The search options
	 * @return Result hits and statistics
	 */
	public GUIResult search(GUISearchOptions options) throws ServerException;

	/**
	 * Saves the search options in the user's working dir
	 * 
	 * @param sid The current user session
	 * @param options The search options
	 * @return true if the operation was successful and there were no duplicates
	 */
	public boolean save(GUISearchOptions options) throws ServerException;

	/**
	 * Deletes a previously saved search
	 * 
	 * @param sid The current user session
	 * @param names The saved search names
	 */
	public void delete(String[] names) throws ServerException;

	/**
	 * Loads a saved search
	 * 
	 * @param sid The current user session
	 * @param name The saved search name
	 * @return
	 */
	public GUISearchOptions load(String name) throws ServerException;

	public static class Instance {
		private static SearchServiceAsync instance;

		public static SearchServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SearchService.class);
			}
			return instance;
		}
	}
}