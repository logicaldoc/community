package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A filed to display long integers in cells that contains a user reference
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class IntegerListGridField extends ColoredListGridField {

	public IntegerListGridField(String name, String title) {
		this(name, I18N.message(title), 40);
	}
	
	public IntegerListGridField(String name, String title, int width) {
		super(name, I18N.message(title), width);
		setType(ListGridFieldType.INTEGER);
		setAlign(Alignment.CENTER);
		setCellFormatter(new LongCellFormatter());
	}

	/**
	 * Utility formatter for those cells that contains longs
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.7
	 */
	public class LongCellFormatter extends ColoredCellFormatter {

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = null;
			if(value instanceof Integer)
				val=Util.formatInt(Integer.parseInt(value.toString()));
			else
				val=Util.formatLong(Long.parseLong(value.toString()));
			return super.format(val, rec, rowNum, colNum);
		}
	}
}