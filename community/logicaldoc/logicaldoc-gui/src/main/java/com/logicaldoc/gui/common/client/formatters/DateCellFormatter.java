package com.logicaldoc.gui.common.client.formatters;

import java.util.Date;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility formatter for those cells that contains dates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DateCellFormatter implements CellFormatter {
	private boolean shortFormat = false;

	@Override
	public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
		if (shortFormat)
			return I18N.formatDateShort((Date) value);
		else
			return I18N.formatDate((Date) value);
	}

	public DateCellFormatter(boolean shortFormat) {
		this.shortFormat = shortFormat;
	}
}