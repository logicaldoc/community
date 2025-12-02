package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;

import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

/**
 * Implementations must convert between {@link GUISearchOptions} and concrete
 * {@link SearchOptions}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class SearchOptionsConverter {

	/**
	 * Converts a {@link SearchOptions}in the GUI counterpart
	 * 
	 * @param searchOptions the search options
	 * 
	 * @returnthe converted options
	 */
	public GUISearchOptions toGUIOptions(SearchOptions searchOptions) {
		GUISearchOptions guiOptions = new GUISearchOptions();
		guiOptions.setType(searchOptions.getType());
		guiOptions.setDescription(searchOptions.getDescription());
		guiOptions.setExpression(searchOptions.getExpression());
		guiOptions.setMaxHits(searchOptions.getMaxHits());
		guiOptions.setName(searchOptions.getName());
		guiOptions.setUserId(searchOptions.getUserId());
		guiOptions.setTopOperator(searchOptions.getTopOperator());
		guiOptions.setFolder(searchOptions.getFolderId());
		guiOptions.setSearchInSubPath(searchOptions.isSearchInSubPath());
		guiOptions.setTemplate(searchOptions.getTemplate());
		guiOptions.setCaseSensitive(searchOptions.isCaseSensitive());
		guiOptions.setRetrieveAliases(searchOptions.isRetrieveAliases());
		guiOptions.setFilterIds(new ArrayList<>(searchOptions.getFilterIds()));
		return guiOptions;
	}

	/**
	 * Converts a {@link GUISearchOptions} into the right search option
	 * 
	 * @param guiOptions the gui options to convert
	 * 
	 * @return converted options
	 */
	public SearchOptions toSearchOptions(GUISearchOptions guiOptions) {
		SearchOptions searchOptions = Search.newOptions(guiOptions.getType());
		searchOptions.setTopOperator(guiOptions.getTopOperator());
		searchOptions.setDescription(guiOptions.getDescription());
		searchOptions.setExpression(guiOptions.getExpression());
		searchOptions.setMaxHits(guiOptions.getMaxHits());
		searchOptions.setName(guiOptions.getName());
		searchOptions.setUserId(guiOptions.getUserId());
		searchOptions.setCaseSensitive(guiOptions.isCaseSensitive());
		searchOptions.setRetrieveAliases(guiOptions.isRetrieveAliases());
		searchOptions.setFolderId(guiOptions.getFolder());
		searchOptions.setSearchInSubPath(guiOptions.isSearchInSubPath());
		searchOptions.setTemplate(guiOptions.getTemplate());
		searchOptions.setFilterIds(new HashSet<>(guiOptions.getFilterIds()));
		return searchOptions;
	}

	protected java.util.Date convertToJavaDate(Date source) {
		if (source == null)
			return source;

		Calendar cal = Calendar.getInstance();
		cal.setTime(source);
		return cal.getTime();
	}
}