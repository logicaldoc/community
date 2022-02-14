package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.SortChangedHandler;
import com.smartgwt.client.widgets.grid.events.SortEvent;

/**
 * Grid of documents displayed in the Navigator
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class NavigatorDocumentsGrid extends DocumentsListGrid {

	private long lastChangedSortFolder = 0L;

	public NavigatorDocumentsGrid(GUIFolder folder) {
		super(folder);
		setSelectionType(SelectionStyle.MULTIPLE);
		this.lastChangedSortFolder = folder.getId();

		int pageSize = loadGridLayout(folder);
		DocumentsDS dataSource = new DocumentsDS(folder, null, pageSize, 1, null, false, false,
				DocumentGridUtil.getSortSpec(this));
		setDataSource(dataSource);
		

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
		if (!fields.contains(fieldsMap.get("pages"))) {
			fieldsMap.get("pages").setHidden(true);
			fields.add(fieldsMap.get("pages"));
		}
		if (!fields.contains(fieldsMap.get("fileVersion"))) {
			fieldsMap.get("fileVersion").setHidden(true);
			fields.add(fieldsMap.get("fileVersion"));
		}
		if (!fields.contains(fieldsMap.get("version"))) {
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
		if (!fields.contains(fieldsMap.get("language"))) {
			fieldsMap.get("language").setHidden(true);
			fields.add(fieldsMap.get("language"));
		}

		setFields(fields.toArray(new ListGridField[0]));

		/*
		 * Restore any previously saved view state for this grid
		 */
		addDrawHandler(new DrawHandler() {
			@Override
			public void onDraw(DrawEvent event) {
				loadGridLayout(folder);
			}
		});

		addSortChangedHandler(new SortChangedHandler() {

			@Override
			public void onSortChanged(SortEvent event) {
				/*
				 * Check if the folder has been changes since the last sorting
				 * and check if we have more than one page
				 */
				if (lastChangedSortFolder == getFolder().getId() && getGridCursor().getTotalPages() > 1) {
					// if we have more pages, it is required to retrieve again
					// the recodrs from the server using the right sorting
					DocumentsDS dataSource = new DocumentsDS(getFolder(), null, getGridCursor().getPageSize(),
							getGridCursor().getCurrentPage(), null, false, false,
							DocumentGridUtil.getSortSpec(event.getSortSpecifiers()));
					refresh(dataSource);
				} else {
					// save the current folder's ID
					lastChangedSortFolder = getFolder().getId();
				}
			}
		});
	}
}