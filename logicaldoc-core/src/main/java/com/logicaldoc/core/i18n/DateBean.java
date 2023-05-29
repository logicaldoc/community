package com.logicaldoc.core.i18n;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * This class converts a date into some formats.
 * 
 * @author Michael Scholz
 * @author Sebastian Stein
 * @version 1.1
 */
public class DateBean extends Date {
	
	private static final String YYYY_MM_DD = "yyyyMMdd";

	private static final String YYYY_MMDD_HMMSS = "yyyyMMdd HHmmss";
	
	private static final long serialVersionUID = 1L;

	/**
	 * This method returns the current date in the format yyyyMMdd HH:mm:ss.
	 * 
	 * @return current date in format yyyyMMdd HH:mm:ss
	 */
	public static String toCompactString() {
		SimpleDateFormat targetFormat = new SimpleDateFormat();
		targetFormat.applyPattern(YYYY_MMDD_HMMSS);

		String result = targetFormat.format(new DateBean());

		return result;
	}

	/**
	 * This method creates the Date object for a compact string in the format
	 * yyyyMMdd HH:mm:ss or yyyyHHdd.
	 * 
	 * @param compactString compact text representation of the date
	 * 
	 * @return the date
	 */
	public static Date dateFromCompactString(String compactString) {
		SimpleDateFormat targetFormat = new SimpleDateFormat(YYYY_MMDD_HMMSS);
		if (compactString.length() < 9)
			targetFormat = new SimpleDateFormat(YYYY_MM_DD);

		try {
			return targetFormat.parse(compactString);
		} catch (ParseException e) {
			return null;
		}
	}

	/**
	 * This method formats a a date into format yyyymmdd.
	 * 
	 * @param date the date to be converted
	 * 
	 * @return a string containing the converted date
	 */
	public static String toCompactString(Date date) {
		DateFormat df = new SimpleDateFormat(YYYY_MM_DD);
		return df.format(date);
	}

	/**
	 * This method formats a string with format dd.mm.yyyy into format yyyymmdd.
	 * 
	 * @param date string containing the date to be converted
	 * @param dateFormat format of the date
	 * 
	 * @return a string containing the converted date
	 */
	public static String toCompactString(String date, String dateFormat) {
		if ((date != null) && !date.equals("")) {
			return convertDate(dateFormat, YYYY_MMDD_HMMSS, date);
		} else {
			return "";
		}
	}

	/**
	 * Converts a string containing a date between the given formats.
	 * 
	 * @param formatIn current format of the string
	 * @param formatOut format the string should be converted to
	 * @param dateIn the string containing a date in the formatIn
	 * 
	 * @return returns the converted string in the formatOut
	 */
	public static String convertDate(String formatIn, String formatOut, String dateIn) {
		try {
			if (dateIn.length() > 8) {
				return (new SimpleDateFormat(formatOut)).format((new SimpleDateFormat(formatIn).parse(dateIn,
						new ParsePosition(0))));
			} else {
				return (new SimpleDateFormat(formatOut)).format((new SimpleDateFormat(YYYY_MM_DD).parse(dateIn,
						new ParsePosition(0))));
			}
		} catch (Exception ex) {
			return null;
		}
	}
}