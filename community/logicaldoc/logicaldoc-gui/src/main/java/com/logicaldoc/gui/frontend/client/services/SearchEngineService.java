package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;

/**
 * The client side stub for the Search Engine Service.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
@RemoteServiceRelativePath("searchengine")
public interface SearchEngineService extends RemoteService {
	/**
	 * Loads a search engine that contains all search engine infos.
	 */
	public GUISearchEngine getInfo() throws ServerException;

	/**
	 * Counts the total number of entries.
	 */
	public long countEntries() throws ServerException;

	/**
	 * Unlocks the indexer.
	 */
	public void unlock() throws ServerException;

	/**
	 * Checks the indexer.
	 */
	public String check() throws ServerException;

	/**
	 * Reschedule all entries for indexing.
	 */
	public void rescheduleAll(boolean dropIndex) throws ServerException;

	/**
	 * Saves search engine settings
	 */
	public void save(GUISearchEngine searchEngine) throws ServerException;

	/**
	 * Changes the activation status of a language
	 */
	public void setLanguageStatus(String language, boolean active) throws ServerException;

	/**
	 * Sets the parser aliases for the given extension. Aliases must be a
	 * comma-separated values.
	 */
	public void setAliases(String extension, String aliases) throws ServerException;

	/**
	 * Reorders the token filters.
	 * 
	 * @param filters the ordered list of the filters
	 * 
	 * @throws ServerException
	 */
	public void reorderTokenFilters(String[] filters) throws ServerException;

	/**
	 * Saves the settings of the specified token filter
	 */
	public void saveTokenFilterSettings(String filter, GUIParameter[] settings) throws ServerException;

	/**
	 * Changes the activation status of a token filter
	 */
	public void setTokenFilterStatus(String language, boolean active) throws ServerException;

	public static class Instance {
		private static SearchEngineServiceAsync instance;

		public static SearchEngineServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SearchEngineService.class);
			}
			return instance;
		}
	}
}