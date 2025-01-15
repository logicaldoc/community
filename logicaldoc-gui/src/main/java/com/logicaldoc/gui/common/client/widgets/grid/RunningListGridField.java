package com.logicaldoc.gui.common.client.widgets.grid;

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

	public RunningListGridField() {
		super("running", " ", 30);
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String color = rec.getAttributeAsString(colorFieldName);
		Boolean running = rec.getAttributeAsBoolean("running");
		if (running == null)
			return "";

		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		if (Boolean.TRUE.equals(running)) {
			content += AwesomeFactory.getIconButtonHTML("cog", null, "running", color, "spin", null);
		} else {
			content += AwesomeFactory.getIconButtonHTML("cog", null, "idle", color, null);
		}
		content += "</div>";
		return content;
	}
}