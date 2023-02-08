package com.logicaldoc.gui.common.client.formatters;

import java.util.Date;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Formats a cell depending on the guest flag
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class UserDateCellFormatter implements CellFormatter {
	@Override
	public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
		if(value==null)
			return "";
		
		UserCellFormatter userFormatter=new UserCellFormatter();
		return userFormatter.format(I18N.formatDateShort((Date) value), rec, rowNum, colNum);
	}
}