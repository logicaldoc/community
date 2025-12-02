package com.logicaldoc.web.service;

import java.util.HashSet;
import java.util.stream.Collectors;

import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

/**
 * Takes care of converting between fulltext options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class FulltextOptionsConverter extends SearchOptionsConverter {

	@Override
	public GUISearchOptions toGUIOptions(SearchOptions searchOptions) {
		GUISearchOptions guiOptions = super.toGUIOptions(searchOptions);

		if (searchOptions instanceof FulltextSearchOptions fulltext) {
			guiOptions.setDateFrom(fulltext.getDateFrom());
			guiOptions.setDateTo(fulltext.getDateTo());
			guiOptions.setCreationFrom(fulltext.getCreationFrom());
			guiOptions.setCreationTo(fulltext.getCreationTo());
			guiOptions.setExpressionLanguage(fulltext.getExpressionLanguage());
			guiOptions.setFields(
					fulltext.getFields().stream().filter(f -> !"title".equals(f)).collect(Collectors.toList()));
			guiOptions.setFormat(fulltext.getFormat());
			guiOptions.setLanguage(fulltext.getLanguage());
			guiOptions.setSizeMax(fulltext.getSizeMax());
			guiOptions.setSizeMin(fulltext.getSizeMin());
		}

		return guiOptions;
	}

	@Override
	public SearchOptions toSearchOptions(GUISearchOptions guiOptions) {
		SearchOptions searchOptions = super.toSearchOptions(guiOptions);

		if (searchOptions instanceof FulltextSearchOptions fulltext) {
			fulltext.setDateFrom(convertToJavaDate(guiOptions.getDateFrom()));
			fulltext.setDateTo(convertToJavaDate(guiOptions.getDateTo()));
			fulltext.setCreationFrom(convertToJavaDate(guiOptions.getCreationFrom()));
			fulltext.setCreationTo(convertToJavaDate(guiOptions.getCreationTo()));
			fulltext.setExpressionLanguage(guiOptions.getExpressionLanguage());
			fulltext.setFields(new HashSet<>(guiOptions.getFields()));
			fulltext.setFormat(guiOptions.getFormat());
			fulltext.setLanguage(guiOptions.getLanguage());
			fulltext.setSizeMax(guiOptions.getSizeMax());
			fulltext.setSizeMin(guiOptions.getSizeMin());
		}
		return searchOptions;
	}
}