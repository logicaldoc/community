package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;

/**
 * The client side stub for the Search Engine Service.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("searchengine")
public interface SearchEngineService extends RemoteService {
	/**
	 * Loads a search engine that contains all search engine details.
	 *
	 * @return details about the engine
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUISearchEngine getInfo() throws ServerException;

	/**
	 * Counts the total number of entries
	 * 
	 * @return number of entries in the index
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public long countEntries() throws ServerException;

	/**
	 * Unlocks the indexer
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void unlock() throws ServerException;

	/**
	 * Checks the indexer
	 * 
	 * @return check report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public String check() throws ServerException;

	/**
	 * Reschedule all entries for indexing
	 * 
	 * @param dropIndex must the index be dropped also?
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void rescheduleAll(boolean dropIndex) throws ServerException;

	/**
	 * Saves search engine settings
	 * 
	 * @param searchEngine the engine to update
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void save(GUISearchEngine searchEngine) throws ServerException;

	/**
	 * Changes the activation status of a language
	 * 
	 * @param language the language to change
	 * @param active the new active status
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setLanguageStatus(String language, boolean active) throws ServerException;

	/**
	 * Sets the parser aliases for the given extension. Aliases must be a
	 * comma-separated values
	 * 
	 * @param extension the file extension e.g.: xml
	 * @param aliases comma-separated list of aliases e.g.: html,htmlx
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setAliases(String extension, String aliases) throws ServerException;

	/**
	 * Reorders the token filters.
	 * 
	 * @param filters the ordered list of the filters
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void reorderTokenFilters(List<String> filters) throws ServerException;

	/**
	 * Saves the settings of the specified token filter
	 * 
	 * @param filter token filter
	 * @param settings parameters
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveTokenFilterSettings(String filter, List<GUIParameter> settings) throws ServerException;

	/**
	 * Changes the activation status of a token filter
	 * 
	 * @param language the language
	 * @param active the new activation status
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void setTokenFilterStatus(String language, boolean active) throws ServerException;

	/**
	 * Launches the purge
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void purge() throws ServerException;

	/**
	 * Executes a low-level query in the fulltext index
	 * 
	 * @param query the query to use
	 * @param page the page to retrieve
	 * @param size the maximum number of entries to retrieve
	 * 
	 * @return the result
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIResult query(String query, int page, int size) throws ServerException;

	/**
	 * Removes a set of entries from the index
	 * 
	 * @param entryIds identifiers of the entries to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void remove(List<Long> entryIds) throws ServerException;

	public static class Instance {
		private static SearchEngineServiceAsync inst;

		private Instance() {
		}

		public static SearchEngineServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(SearchEngineService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}