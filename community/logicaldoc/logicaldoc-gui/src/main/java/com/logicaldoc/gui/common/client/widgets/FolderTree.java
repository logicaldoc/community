package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.data.FoldersDS;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.tree.TreeGrid;

/**
 * TreeGrid showing the folders
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class FolderTree extends TreeGrid {

	public FolderTree() {
		super();
		setBodyStyleName("normal");
		setShowHeader(false);
		setLeaveScrollbarGap(false);
		setManyItemsImage("cubes_all.png");
		setAppImgDir("pieces/16/");
		setCanReorderRecords(true);
		setCanAcceptDroppedRecords(false);
		setCanDragRecordsOut(false);
		setAutoFetchData(true);
		setLoadDataOnDemand(true);
		setDataSource(FoldersDS.get());
		setCanSelectAll(false);
	}
	
	@Override
	protected String getIcon(Record record, boolean defaultState) {
		if ("1".equals(record.getAttribute("type"))) {
			setCustomNodeIcon(record, Util.imageUrl("cube_blue16.png"));
		}
		return super.getIcon(record, defaultState);
	}
}