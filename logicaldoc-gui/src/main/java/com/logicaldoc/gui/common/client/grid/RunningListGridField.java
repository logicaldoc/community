package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display the running status, it must be bound to a boolean column
 * named running
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class RunningListGridField extends ColoredListGridField {

	private static final String RUNNING = "running";

	public RunningListGridField() {
		this(RUNNING);
	}

	public RunningListGridField(String name) {
		super(name, " ", 30);
		setCanFilter(true);
		setCanSort(true);
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setAlign(Alignment.CENTER);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String content = "";
		String color = rec.getAttributeAsString(colorFieldName);
		content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		if (rec.getAttribute(getName()) != null && Boolean.TRUE.equals(rec.getAttributeAsBoolean(getName()))) {
			content += AwesomeFactory.getIconButtonHTML("refresh", null, getName(), color, "spin", null);
		} else {
			content += AwesomeFactory.getIconButtonHTML("refresh", null, "idle", color, null);
		}
		content += "</div>";
		return content;
	}
}