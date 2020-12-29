package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.logicaldoc.gui.frontend.client.document.grid.GridUtil;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * Grid of documents displayed in the search workspace
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class SearchHitsGrid extends DocumentsListGrid {

	private List<ListGridField> fields = new ArrayList<ListGridField>();

	public SearchHitsGrid() {
		super(null);
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSelectionType(Session.get().isAdmin() ? SelectionStyle.MULTIPLE : SelectionStyle.SINGLE);
		setShowRowNumbers(true);

		fieldsMap.get("type").setHidden(true);
		fieldsMap.get("customId").setHidden(true);

		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		String[] cols = Session.get().getInfo().getConfig("gui.search.columns").split(",");
		for (String col : cols) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}

		if (!fields.contains(fieldsMap.get("filename")))
			fields.add(fieldsMap.get("filename"));
		if (!fields.contains(fieldsMap.get("score")))
			fields.add(fieldsMap.get("score"));
		if (!fields.contains(fieldsMap.get("lastModified")))
			fields.add(fieldsMap.get("lastModified"));
		if (!fields.contains(fieldsMap.get("type")))
			fields.add(fieldsMap.get("type"));
		if (!fields.contains(fieldsMap.get("size")))
			fields.add(fieldsMap.get("size"));
		if (!fields.contains(fieldsMap.get("fileVersion")))
			fields.add(fieldsMap.get("fileVersion"));
		if (!fields.contains(fieldsMap.get("version")))
			fields.add(fieldsMap.get("version"));
		if (!fields.contains(fieldsMap.get("publisher")))
			fields.add(fieldsMap.get("publisher"));
		if (!fields.contains(fieldsMap.get("published")))
			fields.add(fieldsMap.get("published"));
		if (!fields.contains(fieldsMap.get("creator")))
			fields.add(fieldsMap.get("creator"));
		if (!fields.contains(fieldsMap.get("created")))
			fields.add(fieldsMap.get("created"));
		if (!fields.contains(fieldsMap.get("customId")))
			fields.add(fieldsMap.get("customId"));
		if (!fields.contains(fieldsMap.get("folder")))
			fields.add(fieldsMap.get("folder"));
		if (!fields.contains(fieldsMap.get("rating")))
			fields.add(fieldsMap.get("rating"));
		if (!fields.contains(fieldsMap.get("rating")))
			fields.add(fieldsMap.get("rating"));
		if (!fields.contains(fieldsMap.get("comment")))
			fields.add(fieldsMap.get("comment"));
		if (!fields.contains(fieldsMap.get("workflowStatus")))
			fields.add(fieldsMap.get("workflowStatus"));
		if (!fields.contains(fieldsMap.get("workflowStatusDisp")))
			fields.add(fieldsMap.get("workflowStatusDisp"));
		if (!fields.contains(fieldsMap.get("startPublishing")))
			fields.add(fieldsMap.get("startPublishing"));
		if (!fields.contains(fieldsMap.get("stopPublishing")))
			fields.add(fieldsMap.get("stopPublishing"));
		if (!fields.contains(fieldsMap.get("template")))
			fields.add(fieldsMap.get("template"));
		if (!fields.contains(fieldsMap.get("language")))
			fields.add(fieldsMap.get("language"));

		setFields(fields.toArray(new ListGridField[0]));

		loadGridLayout(null);
	}

	@Override
	public void loadGridLayout(GUIFolder folder) {
		String previouslySavedState = Session.get().getUser().getHitsGrid();

		Integer pageSize = GridUtil.getPageSizeFromSpec(previouslySavedState);
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("search.hits");
		if (SearchPanel.get().getListingPanel() != null
				&& SearchPanel.get().getListingPanel().getSearchCursor() != null)
			SearchPanel.get().getListingPanel().getSearchCursor().setPageSize(pageSize);

		String gridLayout = GridUtil.getGridLayoutFromSpec(previouslySavedState);
		if (gridLayout != null)
			setViewState(gridLayout);
	}
}