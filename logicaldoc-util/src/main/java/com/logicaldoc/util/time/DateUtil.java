package com.logicaldoc.util.time;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods to handle dates
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class DateUtil {

	private static final String YYYY_MM_DD = "yyyy-MM-dd";

	private static final Logger log = LoggerFactory.getLogger(DateUtil.class);

	private DateUtil() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Formats a date using the ISO format <code>yyyy-MM-dd HH:mm:ss Z</code>
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted date
	 */
	public static String format(Date date) {
		if (date == null)
			return null;

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		return df.format(date);
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
		if (date == null)
			return null;

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS Z");
		return df.format(date);
	}

	/**
	 * Formats a date using one of the ISO formats.
	 * 
	 * @param formattedDate the string to parse
	 * 
	 * @return the parsed date
	 */
	public static Date parse(String formattedDate) {
		Date date = null;
		String[] possibleFormats = new String[] { "yyyy-MM-dd HH:mm:ss.SSS Z", "yyyy-MM-dd HH:mm:ss.SS Z",
				"yyyy-MM-dd HH:mm:ss Z", YYYY_MM_DD };
		for (String format : possibleFormats) {
			try {
				SimpleDateFormat df = new SimpleDateFormat(format);
				date = df.parse(formattedDate);
				if (date != null)
					break;
			} catch (ParseException e) {
				// Nothing to do
			}
		}
		if (date == null)
			log.error("Unparseable date {}", formattedDate);

		return date;
	}

	public static Date truncateToDay(Date date) {
		Instant instant = date.toInstant();
		ZonedDateTime zonedDateTime = instant.atZone(ZoneId.systemDefault());
		ZonedDateTime truncatedZonedDateTime = zonedDateTime.truncatedTo(ChronoUnit.DAYS);
		Instant truncatedInstant = truncatedZonedDateTime.toInstant();
		return Date.from(truncatedInstant);
	}
}