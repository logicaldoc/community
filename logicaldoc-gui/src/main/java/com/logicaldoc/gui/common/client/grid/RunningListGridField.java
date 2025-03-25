package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.util.AwesomeFactory;
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
		super(RUNNING, " ", 30);
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}
	
	public RunningListGridField(String name) {
		super(name, " ", 30);
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String color = rec.getAttributeAsString(colorFieldName);
		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean(getName()))) {
			content += AwesomeFactory.getIconButtonHTML("refresh", null, getName(), color, "spin", null);
		} else {
			content += AwesomeFactory.getIconButtonHTML("refresh", null, "idle", color, null);
		}
		content += "</div>";
		return content;
	}
}