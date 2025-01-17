package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display the enabled/disabled status, it must be bound to a boolean
 * column named eenabled
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class EnabledListGridField extends ListGridField {

	public EnabledListGridField() {
		super("eenabled", " ", 30);
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean("eenabled"))) {
			content += AwesomeFactory.getIconButtonHTML("circle-check", null, "enabled", "green", null);
		} else {
			content += AwesomeFactory.getIconButtonHTML("ban", null, "disabled", "red", null);
		}
		content += "</div>";
		return content;
	}
}