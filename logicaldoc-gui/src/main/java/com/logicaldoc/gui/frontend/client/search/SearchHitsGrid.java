package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * Grid of documents displayed in the search workspace
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class SearchHitsGrid extends DocumentsListGrid {

	private static final String CUSTOM_ID = "customId";
	private List<ListGridField> fields = new ArrayList<>();

	public SearchHitsGrid() {
		super();
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);
		setSelectionType(SelectionStyle.MULTIPLE);
		setShowRowNumbers(true);

		fieldsMap.get("type").setHidden(true);
		fieldsMap.get(CUSTOM_ID).setHidden(true);

		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		getSearchColumns();

		addFilename();
		addScore();
		addDates();

		if (!fields.contains(fieldsMap.get("type")))
			fields.add(fieldsMap.get("type"));
		if (!fields.contains(fieldsMap.get("size")))
			fields.add(fieldsMap.get("size"));
		if (!fields.contains(fieldsMap.get("pages")))
			fields.add(fieldsMap.get("pages"));

		addVersions();

		addUsers();

		if (!fields.contains(fieldsMap.get(CUSTOM_ID)))
			fields.add(fieldsMap.get(CUSTOM_ID));
		if (!fields.contains(fieldsMap.get("folder")))
			fields.add(fieldsMap.get("folder"));
		if (!fields.contains(fieldsMap.get("rating")))
			fields.add(fieldsMap.get("rating"));
		if (!fields.contains(fieldsMap.get("comment")))
			fields.add(fieldsMap.get("comment"));

		addWorkflow();

		if (!fields.contains(fieldsMap.get("template")))
			fields.add(fieldsMap.get("template"));
		if (!fields.contains(fieldsMap.get("language")))
			fields.add(fieldsMap.get("language"));

		setFields(fields.toArray(new ListGridField[0]));

		loadGridLayout(null);
	}

	private void addWorkflow() {
		if (!fields.contains(fieldsMap.get("workflowStatus")))
			fields.add(fieldsMap.get("workflowStatus"));
		if (!fields.contains(fieldsMap.get("workflowStatusDisp")))
			fields.add(fieldsMap.get("workflowStatusDisp"));
	}

	private void addVersions() {
		if (!fields.contains(fieldsMap.get("fileVersion")))
			fields.add(fieldsMap.get("fileVersion"));
		if (!fields.contains(fieldsMap.get("version")))
			fields.add(fieldsMap.get("version"));
	}

	private void addUsers() {
		if (!fields.contains(fieldsMap.get("publisher")))
			fields.add(fieldsMap.get("publisher"));
		if (!fields.contains(fieldsMap.get("creator")))
			fields.add(fieldsMap.get("creator"));
	}

	private void addDates() {
		if (!fields.contains(fieldsMap.get("lastModified")))
			fields.add(fieldsMap.get("lastModified"));
		if (!fields.contains(fieldsMap.get("created")))
			fields.add(fieldsMap.get("created"));
		if (!fields.contains(fieldsMap.get("published")))
			fields.add(fieldsMap.get("published"));
		if (!fields.contains(fieldsMap.get("startPublishing")))
			fields.add(fieldsMap.get("startPublishing"));
		if (!fields.contains(fieldsMap.get("stopPublishing")))
			fields.add(fieldsMap.get("stopPublishing"));
	}

	private void addScore() {
		if (!fields.contains(fieldsMap.get("score")))
			fields.add(fieldsMap.get("score"));
	}

	private void addFilename() {
		if (!fields.contains(fieldsMap.get("filename")))
			fields.add(fieldsMap.get("filename"));
	}

	private void getSearchColumns() {
		String srcColsSpec = Session.get().getInfo().getConfig("gui.search.columns");
		if(srcColsSpec==null || srcColsSpec.isEmpty())
			return;
		
		String[] searchColumns = srcColsSpec.split(",");
		for (String col : searchColumns) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}
	}

	@Override
	public int loadGridLayout(GUIFolder folder) {
		String previouslySavedState = Session.get().getUser().getHitsGrid();

		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(previouslySavedState);
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("search.hits");
		if (SearchPanel.get().getListingPanel() != null
				&& SearchPanel.get().getListingPanel().getSearchCursor() != null)
			SearchPanel.get().getListingPanel().getSearchCursor().setPageSize(pageSize);

		String gridLayout = DocumentGridUtil.getGridLayoutFromSpec(previouslySavedState);
		if (gridLayout != null)
			setViewState(gridLayout);

		return pageSize;
	}
}