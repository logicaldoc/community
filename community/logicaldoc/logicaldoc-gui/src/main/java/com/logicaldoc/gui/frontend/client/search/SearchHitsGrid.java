package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsListGrid;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.GroupStateChangedEvent;
import com.smartgwt.client.widgets.grid.events.GroupStateChangedHandler;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedEvent;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedHandler;

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
		setSelectionType(SelectionStyle.SINGLE);
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

		setFields(fields.toArray(new ListGridField[0]));

		/*
		 * Save the layout of the grid at every change
		 */
		addViewStateChangedHandler(new ViewStateChangedHandler() {
			@Override
			public void onViewStateChanged(ViewStateChangedEvent event) {
				saveGridState();
			}
		});

		/*
		 * Save the grouping of the grid at every change
		 */
		addGroupStateChangedHandler(new GroupStateChangedHandler() {

			@Override
			public void onGroupStateChanged(GroupStateChangedEvent event) {
				saveGridState();
			}
		});

		/*
		 * Restore any previously saved view state for this grid.
		 */
		addDrawHandler(new DrawHandler() {
			@Override
			public void onDraw(DrawEvent event) {
				saveGridState();
			}
		});

		loadGridState();
	}

	private void saveGridState() {
		CookiesManager.save(CookiesManager.COOKIE_HITSLIST, getViewState());
		CookiesManager.save(CookiesManager.COOKIE_HITSLIST_GROUPING, getGroupState());
	}

	private void loadGridState() {
		String previouslySavedState = CookiesManager.get(CookiesManager.COOKIE_HITSLIST);
		if (previouslySavedState != null)
			setViewState(previouslySavedState);
		String previouslySavedGroupState = CookiesManager.get(CookiesManager.COOKIE_HITSLIST_GROUPING);
		if (previouslySavedGroupState != null)
			setGroupState(previouslySavedGroupState);
	}
}
