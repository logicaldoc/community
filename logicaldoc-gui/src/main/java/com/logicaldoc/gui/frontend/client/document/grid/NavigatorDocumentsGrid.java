package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.DocumentsDS;
import com.logicaldoc.gui.common.client.data.DocumentsDSParameters;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;

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
		DocumentsDSParameters params = new DocumentsDSParameters(folder.getId(), null, pageSize, 1,
				DocumentGridUtil.getSortSpec(this));
		setDataSource(new DocumentsDS(params));

		final List<ListGridField> fields = new ArrayList<>();

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

		mergeFields(fields);

		setFields(fields.toArray(new ListGridField[0]));

		/*
		 * Restore any previously saved view state for this grid
		 */
		addDrawHandler(event -> loadGridLayout(folder));

		addSortChangedHandler(event -> {
			/*
			 * Check if the folder has been changes since the last sorting and
			 * check if we have more than one page
			 */
			if (lastChangedSortFolder == getFolder().getId() && getGridCursor().getTotalPages() > 1) {
				// if we have more pages, it is required to retrieve again
				// the records from the server using the right sorting
				DocumentsDSParameters pars = new DocumentsDSParameters(getFolder().getId(), null,
						getGridCursor().getPageSize(), getGridCursor().getCurrentPage(),
						DocumentGridUtil.getSortSpec(event.getSortSpecifiers()));
				refresh(new DocumentsDS(pars));
			} else {
				// save the current folder's ID
				lastChangedSortFolder = getFolder().getId();
			}
		});

		/**
		 * When this grid is first drawn we noted that the
		 * DocumentGridUtil.getSortSpec returns null and so the grid does not
		 * get sorted, so we call the FolderNavigator.get().selectFolder and
		 * then the grid displays correctly
		 */
		FolderNavigator.get().selectFolder(folder.getId());
	}
	
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}