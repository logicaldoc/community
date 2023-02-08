package com.logicaldoc.gui.frontend.client.sharefile;

import com.logicaldoc.gui.common.client.data.ShareFileDS;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * TreeGrid showing the folders and files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class ShareFileTree extends TreeGrid {

	public ShareFileTree(boolean export) {
		super();

		setBodyStyleName("normal");
		setShowHeader(false);
		setLeaveScrollbarGap(false);
		setCanReorderRecords(false);
		setCanDragRecordsOut(false);
		setAutoFetchData(true);
		setLoadDataOnDemand(true);
		setDataSource(new ShareFileDS(export));
		setCanSelectAll(false);
		setShowConnectors(true);
		setCanAcceptDrop(false);
		setCanAcceptDroppedRecords(false);
		setShowRoot(false);

		if (export)
			setSelectionType(SelectionStyle.SINGLE);
		else {
			setSelectionType(SelectionStyle.MULTIPLE);
			setSelectionAppearance(SelectionAppearance.CHECKBOX);
		}

		addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				TreeNode node = getTree().getChildren(getTree().getRoot())[0];
				getTree().openFolder(node);
			}
		});

	}

	@Override
	protected String getIcon(Record rec, boolean defaultState) {
		if (!"folder".equals(rec.getAttributeAsString("iicon"))) {
			setCustomNodeIcon(rec, Util.imageUrl(rec.getAttribute("iicon") + ".png"));
		}
		return super.getIcon(rec, defaultState);
	}
}