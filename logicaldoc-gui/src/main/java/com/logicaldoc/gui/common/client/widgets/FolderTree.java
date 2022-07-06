package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.widgets.grid.FolderListGridField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.events.FolderClosedEvent;
import com.smartgwt.client.widgets.tree.events.FolderClosedHandler;
import com.smartgwt.client.widgets.tree.events.FolderOpenedEvent;
import com.smartgwt.client.widgets.tree.events.FolderOpenedHandler;

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

		addFolderOpenedHandler(new FolderOpenedHandler() {

			@Override
			public void onFolderOpened(FolderOpenedEvent event) {
				event.getNode().setAttribute("opened", true);
			}

		});

		addFolderClosedHandler(new FolderClosedHandler() {

			@Override
			public void onFolderClosed(FolderClosedEvent event) {
				event.getNode().setAttribute("opened", false);
			}
		});
	}

	@Override
	protected String getIcon(Record record, boolean defaultState) {
		return "blank.gif";
	}
}