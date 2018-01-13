package com.logicaldoc.gui.common.client.formatters;

import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility formatter for those cells that contains file sizes
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class FileSizeCellFormatter implements CellFormatter {
	private boolean win7format = false;

	public FileSizeCellFormatter() {
		super();
	}
	
	public FileSizeCellFormatter(boolean win7format) {
		super();
		this.win7format = win7format;
	}

	@Override
	public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
		if (win7format)
			return Util.formatSizeW7(value);
		else
			return Util.formatSizeKB(value);
	}
}
