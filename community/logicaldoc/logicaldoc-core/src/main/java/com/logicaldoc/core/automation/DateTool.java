package com.logicaldoc.core.automation;

import java.util.Date;

import com.ibm.icu.text.SimpleDateFormat;

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
		this.dateFormatLong = dateFormatLong;
		this.dateFormatShort = dateFormatShort;
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
}