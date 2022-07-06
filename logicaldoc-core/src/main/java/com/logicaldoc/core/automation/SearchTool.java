package com.logicaldoc.core.automation;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.searchengine.SearchOptions;

/**
 * Utility methods to do searches from within Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
@AutomationDictionary
public class SearchTool {

	protected static Logger log = LoggerFactory.getLogger(SearchTool.class);

	/**
	 * Factory method for searches.
	 * 
	 * @param options the search criteria
	 * 
	 * @return a {@link Search} instance for the given search options
	 */
	public Search newSearch(SearchOptions options) {
		return Search.get(options);
	}

	/**
	 * Instantiates a new {@link Search}
	 * 
	 * @param options the search criteria
	 * 
	 * @return The list of hits that satisfy the search criteria
	 * 
	 * @throws SearchException Raised in case of an error during the search
	 */
	public List<Hit> search(SearchOptions options) throws SearchException {
		return newSearch(options).search();
	}
}