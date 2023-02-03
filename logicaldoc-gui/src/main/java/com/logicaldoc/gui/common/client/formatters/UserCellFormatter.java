package com.logicaldoc.gui.common.client.formatters;

import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Formats a cell containing a date of a user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class UserCellFormatter implements CellFormatter {
	private static final String CLOSE_SPAN = "</span>";

	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return null;
		if (rec.getAttributeAsBoolean("guest") != null && rec.getAttributeAsBoolean("guest"))
			if (Boolean.TRUE.equals(rec.getAttributeAsBoolean("eenabled")))
				return "<span style='color: #888888;'>" + value + CLOSE_SPAN;
			else
				return "<span style='color: #cc8888;'>" + value + CLOSE_SPAN;
		else if (Boolean.TRUE.equals(rec.getAttributeAsBoolean("eenabled")))
			return value.toString();
		else
			return "<span style='color: red;'>" + value + CLOSE_SPAN;
	}
}