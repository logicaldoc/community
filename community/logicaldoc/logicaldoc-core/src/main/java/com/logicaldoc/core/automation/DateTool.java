package com.logicaldoc.core.automation;

import java.util.Date;

import com.ibm.icu.text.SimpleDateFormat;
import com.logicaldoc.i18n.I18N;

/**
 * Utility class to handle dates from inside Velocity macros
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class DateTool extends org.apache.velocity.tools.generic.DateTool {
	private String dateFormatLong;

	private String dateFormatShort;

	public DateTool(String dateFormatLong, String dateFormatShort) {
		super();
		this.dateFormatLong = dateFormatLong != null ? dateFormatLong : I18N.message("format_date");
		this.dateFormatShort = dateFormatShort != null ? dateFormatShort : I18N.message("format_dateshort");
	}

	public DateTool() {
		super();
		this.dateFormatLong = I18N.message("format_date");
		this.dateFormatShort = I18N.message("format_dateshort");
	}

	public String format(Date date, boolean time) {
		if (time) {
			SimpleDateFormat df = new SimpleDateFormat(dateFormatLong);
			return df.format(date);
		} else {
			SimpleDateFormat df = new SimpleDateFormat(dateFormatShort);
			return df.format(date);
		}
	}

	public String getDateFormatShort() {
		return dateFormatShort;
	}

	public void setDateFormatShort(String dateFormatShort) {
		this.dateFormatShort = dateFormatShort;
	}

	public String getDateFormatLong() {
		return dateFormatLong;
	}

	public void setDateFormatLong(String dateFormatLong) {
		this.dateFormatLong = dateFormatLong;
	}
}