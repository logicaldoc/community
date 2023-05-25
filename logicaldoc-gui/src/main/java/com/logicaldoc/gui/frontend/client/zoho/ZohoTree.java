package com.logicaldoc.gui.frontend.client.zoho;

import com.logicaldoc.gui.common.client.data.ZohoDS;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionAppearance;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;

/**
 * TreeGrid showing the folders and files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.2
 */
public class ZohoTree extends TreeGrid {

	public ZohoTree(boolean export) {
		super();

		setBodyStyleName("normal");
		setShowHeader(false);
		setLeaveScrollbarGap(false);
		setCanReorderRecords(false);
		setCanDragRecordsOut(false);
		setAutoFetchData(true);
		setLoadDataOnDemand(true);
		setDataSource(new ZohoDS(null, export));
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

		addDataArrivedHandler((DataArrivedEvent event) -> {
			TreeNode node = getTree().getChildren(getTree().getRoot())[0];
			getTree().openFolder(node);
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