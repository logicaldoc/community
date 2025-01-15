package com.logicaldoc.gui.common.client.widgets.grid.formatters;

import java.util.Date;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Formats a cell showing a date for the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class UserDateCellFormatter implements CellFormatter {

	private boolean shortFormat = true;

	public UserDateCellFormatter(boolean shortFormat) {
		super();
		this.shortFormat = shortFormat;
	}
	
	public UserDateCellFormatter() {
		super();
	}

	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if (value == null)
			return "";

		UserCellFormatter userFormatter = new UserCellFormatter();
		return userFormatter.format(shortFormat ? I18N.formatDateShort((Date) value) : I18N.formatDate((Date) value),
				rec, rowNum, colNum);
	}
}