package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.grid.FolderListGridField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * TreeGrid showing the folders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderTree extends TreeGrid {

	public FolderTree() {
		super();
		setBodyStyleName("normal");
		setShowHeader(false);
		setLeaveScrollbarGap(false);
		setCanReorderRecords(true);
		setCanAcceptDroppedRecords(false);
		setCanDragRecordsOut(false);
		setAutoFetchData(true);
		setLoadDataOnDemand(true);
		setDataSource(new FoldersDS("foldertree", true));
		setCanSelectAll(false);
		setIconSize(1);

		ListGridField name = new FolderListGridField();
		setFields(name);

		addFolderOpenedHandler(event -> event.getNode().setAttribute("opened", true));

		addFolderClosedHandler(event -> event.getNode().setAttribute("opened", false));
	}

	@Override
	protected String getIcon(Record rec, boolean defaultState) {
		return "blank.gif";
	}
}