package com.logicaldoc.gui.common.client.widgets.grid.formatters;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility formatter for those cells that contains a days number
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DaysCellFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return null;
		if (value instanceof Long longVal)
			return longVal.toString() + " " + (longVal > 1 ? I18N.message("ddays") : I18N.message("day"));
		else if (value instanceof Integer intVal)
			return value.toString() + " " + (intVal > 1 ? I18N.message("ddays") : I18N.message("day"));
		else
			return value.toString();
	}
}
