package com.logicaldoc.util.time;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods to handle dates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class DateUtil {

	protected static Logger log = LoggerFactory.getLogger(DateUtil.class);

	/**
	 * Formats a date using the ISO format <code>yyyy-MM-dd HH:mm:ss Z</code>
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted date
	 */
	public static String format(Date date) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		try {
			return df.format(date);
		} catch (Exception e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			try {
				return df.format(date);
			} catch (Exception e1) {
			}
		}
		return null;
	}

	/**
	 * Formats a date using the ISO format
	 * <code>yyyy-MM-dd HH:mm:ss.SSS Z</code>
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted date
	 */
	public static String formatWithMillis(Date date) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS Z");
		try {
			return df.format(date);
		} catch (Exception e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			try {
				return df.format(date);
			} catch (Exception e1) {
			}
		}
		return null;
	}

	/**
	 * Formats a date using one of the ISO formats.
	 * 
	 * @param formattedDate the string to parse
	 * 
	 * @return the parsed date
	 */
	public static Date parse(String formattedDate) {
		if (StringUtils.isEmpty(formattedDate))
			return null;

		Date date = null;
		String[] possibleFormats = new String[] { "yyyy-MM-dd HH:mm:ss.SSS Z", "yyyy-MM-dd HH:mm:ss.SS Z",
				"yyyy-MM-dd HH:mm:ss Z", "yyyy-MM-dd" };
		for (String format : possibleFormats) {
			try {
				SimpleDateFormat df = new SimpleDateFormat(format);
				date = df.parse(formattedDate);
			} catch (ParseException e) {

			}
		}
		if (date == null)
			log.error("Unparseable date {}", formattedDate);

		return date;
	}
}
