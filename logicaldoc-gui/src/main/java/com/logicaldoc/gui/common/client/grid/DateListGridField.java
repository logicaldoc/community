package com.logicaldoc.gui.common.client.grid;

import java.util.Date;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A filed to display dates in cells that contains a date
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class DateListGridField extends ColoredListGridField {

	public DateListGridField(String name, String title, int format) {
		super(name, I18N.message(title));
		setAlign(Alignment.CENTER);
		setType(ListGridFieldType.DATE);
		setCellFormatter(new DateCellFormatter(format));
		setCanFilter(false);
		setCanSort(true);
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		
		if (format == DateCellFormatter.FORMAT_LONG)
			setMinWidth(150);
		else if (format == DateCellFormatter.FORMAT_SHORT)
			setMinWidth(90);
		else
			setMinWidth(120);
	}

	public DateListGridField(String name, String title) {
		this(name, I18N.message(title), DateCellFormatter.FORMAT_DEFAULT);
	}

	/**
	 * Utility formatter for those cells that contains dates
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 6.0
	 */
	public class DateCellFormatter extends ColoredCellFormatter {

		public static final int FORMAT_DEFAULT = 0;

		public static final int FORMAT_SHORT = 1;

		public static final int FORMAT_LONG = 2;

		private int format = FORMAT_DEFAULT;

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if(value==null)
				return "";
			
			String val = null;
			if (format == FORMAT_SHORT)
				val = I18N.formatDateShort((Date) value);
			else if (format == FORMAT_LONG)
				val = I18N.formatDateLong((Date) value);
			else
				val = I18N.formatDate((Date) value);
			
			if (val != null)
				return super.format(val, rec, rowNum, colNum);
			else
				return "";
		}

		public DateCellFormatter(int format) {
			this.format = format;
		}

		public DateCellFormatter() {
			this.format = FORMAT_DEFAULT;
		}
	}
}