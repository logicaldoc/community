package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.SortChangedHandler;
import com.smartgwt.client.widgets.grid.events.SortEvent;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedEvent;
import com.smartgwt.client.widgets.grid.events.ViewStateChangedHandler;

/**
 * Grid of documents displayed in the Navigator
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class NavigatorDocumentsGrid extends DocumentsListGrid {

	public NavigatorDocumentsGrid(DocumentsDS ds, GUIFolder folder) {
		super(folder, folder.getDocumentCount());
		setDataSource(ds);
		setSelectionType(SelectionStyle.MULTIPLE);

		final List<ListGridField> fields = new ArrayList<ListGridField>();

		fields.add(fieldsMap.get("id"));
		fields.add(fieldsMap.get("thumbnail"));
		fields.add(fieldsMap.get("statusIcons"));
		fields.add(fieldsMap.get("icon"));

		String[] cols = Session.get().getInfo().getConfig("gui.document.columns").split(",");
		for (String col : cols) {
			ListGridField field = fieldsMap.get(col);
			if (field != null) {
				field.setHidden(false);
				fields.add(field);
			}
		}

		if (!fields.contains(fieldsMap.get("filename"))) {
			fieldsMap.get("filename").setHidden(true);
			fields.add(fieldsMap.get("filename"));
		}

		if (!fields.contains(fieldsMap.get("lastModified"))) {
			fieldsMap.get("lastModified").setHidden(true);
			fields.add(fieldsMap.get("lastModified"));
		}
		if (!fields.contains(fieldsMap.get("type"))) {
			fieldsMap.get("type").setHidden(true);
			fields.add(fieldsMap.get("type"));
		}
		if (!fields.contains(fieldsMap.get("size"))) {
			fieldsMap.get("size").setHidden(true);
			fields.add(fieldsMap.get("size"));
		}
		if (!fields.contains(fieldsMap.get("fileVersion"))) {
			fieldsMap.get("fileVersion").setHidden(true);
			fields.add(fieldsMap.get("fileVersion"));
		}
		if (!fields.contains(fieldsMap.get("vers0ion"))) {
			fieldsMap.get("version").setHidden(true);
			fields.add(fieldsMap.get("version"));
		}
		if (!fields.contains(fieldsMap.get("publisher"))) {
			fieldsMap.get("publisher").setHidden(true);
			fields.add(fieldsMap.get("publisher"));
		}
		if (!fields.contains(fieldsMap.get("published"))) {
			fieldsMap.get("published").setHidden(true);
			fields.add(fieldsMap.get("published"));
		}
		if (!fields.contains(fieldsMap.get("creator"))) {
			fieldsMap.get("creator").setHidden(true);
			fields.add(fieldsMap.get("creator"));
		}
		if (!fields.contains(fieldsMap.get("created"))) {
			fieldsMap.get("created").setHidden(true);
			fields.add(fieldsMap.get("created"));
		}
		if (!fields.contains(fieldsMap.get("customId"))) {
			fieldsMap.get("customId").setHidden(true);
			fields.add(fieldsMap.get("customId"));
		}
		if (!fields.contains(fieldsMap.get("rating"))) {
			fieldsMap.get("rating").setHidden(true);
			fields.add(fieldsMap.get("rating"));
		}
		if (!fields.contains(fieldsMap.get("comment"))) {
			fieldsMap.get("comment").setHidden(true);
			fields.add(fieldsMap.get("comment"));
		}
		if (!fields.contains(fieldsMap.get("workflowStatus"))) {
			fieldsMap.get("workflowStatus").setHidden(true);
			fields.add(fieldsMap.get("workflowStatus"));
		}
		if (!fields.contains(fieldsMap.get("template"))) {
			fieldsMap.get("template").setHidden(true);
			fields.add(fieldsMap.get("template"));
		}
		if (!fields.contains(fieldsMap.get("startPublishing"))) {
			fieldsMap.get("startPublishing").setHidden(true);
			fields.add(fieldsMap.get("startPublishing"));
		}
		if (!fields.contains(fieldsMap.get("stopPublishing"))) {
			fieldsMap.get("stopPublishing").setHidden(true);
			fields.add(fieldsMap.get("stopPublishing"));
		}

		setFields(fields.toArray(new ListGridField[0]));

		addSortChangedHandler(new SortChangedHandler() {

			@Override
			public void onSortChanged(SortEvent event) {
				CookiesManager.save(CookiesManager.COOKIE_DOCSLIST, getViewState());
				saveSorting();
				if (Session.get().getCurrentFolder() != null)
					FolderNavigator.get().selectFolder(Session.get().getCurrentFolder().getId());
			}
		});

		/*
		 * Save the layout of the grid at every change
		 */
		addViewStateChangedHandler(new ViewStateChangedHandler() {
			@Override
			public void onViewStateChanged(ViewStateChangedEvent event) {
				CookiesManager.save(CookiesManager.COOKIE_DOCSLIST, getViewState());
			}
		});

		/*
		 * Restore any previously saved view state for this grid.
		 */
		addDrawHandler(new DrawHandler() {
			@Override
			public void onDraw(DrawEvent event) {
				String previouslySavedState = CookiesManager.get(CookiesManager.COOKIE_DOCSLIST);
				if (previouslySavedState != null)
					setViewState(previouslySavedState);
			}
		});
	}

	private String saveSorting() {
		SortSpecifier[] sortSpecifiers = getSort();
		String sort = "";
		if (sortSpecifiers != null)
			for (SortSpecifier spec : sortSpecifiers) {
				if (!sort.isEmpty())
					sort += ",";
				sort += spec.getField();
				sort += "ascending".equals(spec.getSortDirection().toString().toLowerCase()) ? " asc" : " desc";
			}
		CookiesManager.save(CookiesManager.COOKIE_DOCSLIST_SORT, sort);
		return sort;
	}

}
