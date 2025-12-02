package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.searchengine.folder.FolderCriterion;
import com.logicaldoc.core.searchengine.folder.FolderSearchOptions;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

/**
 * Takes care of converting between fulltext options
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class FolderOptionsConverter extends SearchOptionsConverter {

	@Override
	public GUISearchOptions toGUIOptions(SearchOptions searchOptions) {
		GUISearchOptions guiOptions = super.toGUIOptions(searchOptions);

		if (searchOptions instanceof FolderSearchOptions folder) {
			List<GUICriterion> criteria = new ArrayList<>();
			for (FolderCriterion criterion : folder.getCriteria()) {
				GUICriterion guiCriterion = new GUICriterion();
				guiCriterion.setField(criterion.getField());
				if (criterion.getType() == Attribute.TYPE_DATE)
					guiCriterion.setDateValue(criterion.getDateValue());
				else if (criterion.getType() == Attribute.TYPE_INT || criterion.getType() == FolderCriterion.TYPE_FOLDER
						|| criterion.getType() == Attribute.TYPE_USER || criterion.getType() == Attribute.TYPE_BOOLEAN)
					guiCriterion.setLongValue(criterion.getLongValue());
				else if (criterion.getType() == Attribute.TYPE_DOUBLE)
					guiCriterion.setDoubleValue(criterion.getDoubleValue());
				else if (criterion.getType() == Attribute.TYPE_STRING
						|| criterion.getType() == FolderCriterion.TYPE_LANGUAGE)
					guiCriterion.setStringValue(criterion.getStringValue());

				guiCriterion.setOperator(criterion.getOperator().toLowerCase());

				if (!"folder".equals(guiCriterion.getField()))
					criteria.add(guiCriterion);
			}
			guiOptions.setCriteria(criteria);
		}

		return guiOptions;
	}

	@Override
	public SearchOptions toSearchOptions(GUISearchOptions guiOptions) {
		SearchOptions searchOptions = super.toSearchOptions(guiOptions);

		if (searchOptions instanceof FolderSearchOptions folder) {
			List<FolderCriterion> criteria = new ArrayList<>();
			for (GUICriterion guiCriterion : guiOptions.getCriteria()) {
				FolderCriterion criterion = new FolderCriterion();
				criterion.setField(guiCriterion.getField());
				criterion.setComposition(guiOptions.getTopOperator());

				String operator = null;
				if ("icontains".equals(guiCriterion.getOperator()) || "inotcontains".equals(guiCriterion.getOperator()))
					operator = guiCriterion.getOperator().substring(1);
				else
					operator = guiCriterion.getOperator();

				criterion.setOperator(operator);

				if (guiCriterion.getLongValue() != null) {
					criterion.setLongValue(guiCriterion.getLongValue());
				} else if (guiCriterion.getDateValue() != null) {
					criterion.setDateValue(convertToJavaDate(guiCriterion.getDateValue()));
				} else if (guiCriterion.getDoubleValue() != null) {
					criterion.setDoubleValue(guiCriterion.getDoubleValue());
				} else {
					criterion.setValue(guiCriterion.getStringValue());
				}

				criteria.add(criterion);
			}
			folder.setCriteria(criteria);
		}
		return searchOptions;
	}
}