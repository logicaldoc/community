package com.logicaldoc.gui.common.client.formatters;

import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Formats a cell depending on the guest flag
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class UserCellFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
		if (value == null)
			return null;
		if (record.getAttributeAsBoolean("guest") != null && record.getAttributeAsBoolean("guest"))
			if("0".equals(record.getAttribute("eenabled")))
			   return "<span style='color: #888888;'>" + value + "</span>";
			else
			   return "<span style='color: #cc8888;'>" + value + "</span>";
		else
			if("0".equals(record.getAttribute("eenabled")))
			   return value.toString();
			else
			   return "<span style='color: red;'>" + value + "</span>";
	}
}