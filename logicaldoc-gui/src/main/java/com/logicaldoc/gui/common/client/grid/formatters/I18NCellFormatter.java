package com.logicaldoc.gui.common.client.grid.formatters;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility formatter for those cells that contains internationalized messages
 * (the content is an i18n message key).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class I18NCellFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return null;
		return I18N.message((String) value);
	}
}
