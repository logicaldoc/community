package com.logicaldoc.gui.common.client.grid.formatters;

import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Formats a cell with special representation in case of disabled record
 * (attribute eenabled=false)
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class EnabledCellFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return null;
		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean("eenabled"))) {
			return value.toString();
		} else {
			return "<span style='color: red;'>" + value + "</span>";
		}
	}
}