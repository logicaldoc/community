package com.logicaldoc.core.automation;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.velocity.tools.generic.ComparisonDateTool;

import com.logicaldoc.i18n.I18N;

/**
 * Utility class to handle dates from inside the Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
@AutomationDictionary(key = "DateTool")
public class AutomationDateTool extends org.apache.velocity.tools.generic.DateTool {

	private static final String FORMAT_DATESHORT = "format_dateshort";

	private static final String FORMAT_DATELONG = "format_datelong";

	private static final String FORMAT_DATE = "format_date";

	private static final long serialVersionUID = 1L;

	private String dateFormatLong;

	private String dateFormatShort;

	private String dateFormat;

	private TimeZone timeZone;

	public AutomationDateTool(Locale locale) {
		this(I18N.message(FORMAT_DATE, locale), I18N.message(FORMAT_DATELONG, locale),
				I18N.message(FORMAT_DATESHORT, locale));
	}

	public AutomationDateTool(String dateFormat, String dateFormatLong, String dateFormatShort) {
		super();
		this.dateFormat = dateFormat != null ? dateFormat : I18N.message(FORMAT_DATE);
		this.dateFormatLong = dateFormatLong != null ? dateFormatLong : I18N.message(FORMAT_DATELONG);
		this.dateFormatShort = dateFormatShort != null ? dateFormatShort : I18N.message(FORMAT_DATESHORT);
	}

	public AutomationDateTool() {
		super();
		this.dateFormat = I18N.message(FORMAT_DATE);
		this.dateFormatLong = I18N.message(FORMAT_DATELONG);
		this.dateFormatShort = I18N.message(FORMAT_DATESHORT);
	}

	/**
	 * Formats a date
	 * 
	 * @param date the date to format
	 * @param timeZone the time zone
	 * @param time if the output must contain the time also
	 * 
	 * @return the formatted string
	 */
	public String format(Date date, String timeZone, boolean time) {
		if (date == null)
			return "";

		if (time) {
			SimpleDateFormat df = new SimpleDateFormat(dateFormat);
			if (StringUtils.isNotEmpty(timeZone))
				df.setTimeZone(TimeZone.getTimeZone(timeZone));
			else if (this.timeZone != null)
				df.setTimeZone(this.timeZone);
			return df.format(date);
		} else {
			SimpleDateFormat df = new SimpleDateFormat(dateFormatShort);
			return df.format(date);
		}
	}

	/**
	 * Formats a date
	 * 
	 * @param date the date to format
	 * @param time if the output must contain the time also
	 * 
	 * @return the formatted string
	 */
	public String format(Date date, boolean time) {
		return format(date, null, time);
	}

	/**
	 * Formats a date using the normal format that also includes the time
	 * 
	 * @param date the date to format
	 * @param timeZone the time zone
	 * 
	 * @return the formatted string
	 */
	public String formatDate(Date date, String timeZone) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat(dateFormat);
		if (StringUtils.isNotEmpty(timeZone))
			df.setTimeZone(TimeZone.getTimeZone(timeZone));
		else if (this.timeZone != null)
			df.setTimeZone(this.timeZone);
		return df.format(date);
	}

	/**
	 * Formats a date using the normal format that also includes the time
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted string
	 */
	public String formatDate(Date date) {
		return formatDate(date, null);
	}

	/**
	 * Formats a date using the short format that just includes the date
	 * 
	 * @param date the date to format
	 * @param timeZone the time zone
	 * 
	 * @return the formatted string
	 */
	public String formatDateShort(Date date, String timeZone) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat(dateFormatShort);
		if (StringUtils.isNotEmpty(timeZone))
			df.setTimeZone(TimeZone.getTimeZone(timeZone));
		else if (this.timeZone != null)
			df.setTimeZone(this.timeZone);
		return df.format(date);
	}

	/**
	 * Formats a date using the short format that just includes the date
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted string
	 */
	public String formatDateShort(Date date) {
		return formatDateShort(date, null);
	}

	/**
	 * Formats a date using the long format that includes the time and
	 * milliseconds
	 * 
	 * @param date the date to format
	 * @param timeZone the time zone
	 * 
	 * @return the formatted string
	 */
	public String formatDateLong(Date date, String timeZone) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat(dateFormatLong);
		if (StringUtils.isNotEmpty(timeZone))
			df.setTimeZone(TimeZone.getTimeZone(timeZone));
		else if (this.timeZone != null)
			df.setTimeZone(this.timeZone);
		return df.format(date);
	}

	/**
	 * Formats a date using the long format that includes the time and
	 * milliseconds
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted string
	 */
	public String formatDateLong(Date date) {
		return formatDateLong(date, null);
	}

	/**
	 * Formats a date using the ISO format: <code>yyyy-MM-dd'T'HH:mm:ss</code>
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted string
	 */
	public String formatISO(Date date) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));
		return df.format(date);
	}

	/**
	 * Formats a date using the SQL format: <code>yyyy-MM-dd</code>
	 * 
	 * @param date the date to format
	 * 
	 * @return the formatted string
	 */
	public String formatSQL(Date date) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		return df.format(date);
	}

	/**
	 * Formats a date using a specific format
	 * 
	 * @see java.text.SimpleDateFormat
	 * 
	 * @param date the date to format
	 * @param format the format to use eg: dd/MM/yyyy
	 * @param timeZone the time zone
	 * 
	 * @return the formatted string
	 */
	public String format(Date date, String format, String timeZone) {
		if (date == null)
			return "";

		SimpleDateFormat df = new SimpleDateFormat(format);
		if (StringUtils.isNotEmpty(timeZone))
			df.setTimeZone(TimeZone.getTimeZone(timeZone));
		else if (this.timeZone != null)
			df.setTimeZone(this.timeZone);
		return df.format(date);
	}

	/**
	 * Formats a date using a specific format
	 * 
	 * @see java.text.SimpleDateFormat
	 * 
	 * @param date the date to format
	 * @param format the format to use eg: dd/MM/yyyy
	 * 
	 * @return the formatted string
	 */
	public String format(Date date, String format) {
		return format(date, format, null);
	}

	/**
	 * Parses a string and to retrieve a date
	 * 
	 * @see java.text.SimpleDateFormat
	 * 
	 * @param date string to parse
	 * @param format the format to use
	 * 
	 * @return the parsed date
	 */
	public Date parse(String date, String format) {
		SimpleDateFormat df = new SimpleDateFormat(format);
		try {
			return df.parse(date);
		} catch (ParseException e) {
			return null;
		}
	}

	/**
	 * Retrieves the default date format without time specification
	 * 
	 * @return the default date short format
	 */
	public String getDateFormatShort() {
		return dateFormatShort;
	}

	/**
	 * Sets the default short format(date without time)
	 * 
	 * @param dateFormatShort the short format
	 */
	public void setDateFormatShort(String dateFormatShort) {
		this.dateFormatShort = dateFormatShort;
	}

	/**
	 * Retrieves the default date format with time specification adn
	 * milliseconds
	 * 
	 * @return the default date long format
	 */
	public String getDateFormatLong() {
		return dateFormatLong;
	}

	/**
	 * Sets the default long format(date with time)
	 * 
	 * @param dateFormatLong the long format
	 */
	public void setDateFormatLong(String dateFormatLong) {
		this.dateFormatLong = dateFormatLong;
	}

	/**
	 * Gets the current time
	 * 
	 * @return the current time
	 */
	public Date currentTime() {
		return new Date();
	}

	/**
	 * Adds a given amount of years to a date(you can add negative values to go
	 * back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of years
	 * 
	 * @return the new date
	 */
	public Date addYears(Date src, int amount) {
		return DateUtils.addYears(src, amount);
	}

	/**
	 * Adds a given amount of months to a date(you can add negative values to go
	 * back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of months
	 * 
	 * @return the new date
	 */
	public Date addMonths(Date src, int amount) {
		return DateUtils.addMonths(src, amount);
	}

	/**
	 * Adds a given amount of days to a date(you can add negative values to go
	 * back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of days
	 * 
	 * @return the new date
	 */
	public Date addDays(Date src, int amount) {
		return DateUtils.addDays(src, amount);
	}

	/**
	 * Adds a given amount of minutes to a date(you can add negative values to
	 * go back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of minutes
	 * 
	 * @return the new date
	 */
	public Date addMinutes(Date src, int amount) {
		return DateUtils.addMinutes(src, amount);
	}

	/**
	 * Adds a given amount of seconds to a date(you can add negative values to
	 * go back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of seconds
	 * 
	 * @return the new date
	 */
	public Date addSeconds(Date src, int amount) {
		return DateUtils.addSeconds(src, amount);
	}

	/**
	 * Adds a given amount of milliseconds to a date(you can add negative values
	 * to go back in time)
	 * 
	 * @param src the date to use
	 * @param amount number of milliseconds
	 * 
	 * @return the new date
	 */
	public Date addMilliseconds(Date src, int amount) {
		return DateUtils.addMilliseconds(src, amount);
	}

	/**
	 * Calculates how many days are contained in a given amount of milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of days
	 */
	public long toDays(long ms) {
		return ComparisonDateTool.toDays(ms);
	}

	/**
	 * Calculates how many hours are contained in a given amount of milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of hours
	 */
	public long toHours(long ms) {
		return ComparisonDateTool.toHours(ms);
	}

	/**
	 * Calculates how many minutes are contained in a given amount of
	 * milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of minutes
	 */
	public long toMinutes(long ms) {
		return ComparisonDateTool.toMinutes(ms);
	}

	/**
	 * Calculates how many months are contained in a given amount of
	 * milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of months
	 */
	public long toMonths(long ms) {
		return ComparisonDateTool.toMonths(ms);
	}

	/**
	 * Calculates how many seconds are contained in a given amount of
	 * milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of seconds
	 */
	public long toSeconds(long ms) {
		return ComparisonDateTool.toSeconds(ms);
	}

	/**
	 * Calculates how many weeks are contained in a given amount of milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of weeks
	 */
	public long toWeeks(long ms) {
		return ComparisonDateTool.toWeeks(ms);
	}

	/**
	 * Calculates how many years are contained in a given amount of milliseconds
	 * 
	 * @param ms the milliseconds
	 * 
	 * @return number of years
	 */
	public long toYears(long ms) {
		return ComparisonDateTool.toYears(ms);
	}

	public String getDateFormat() {
		return dateFormat;
	}

	public void setDateFormat(String dateFormat) {
		this.dateFormat = dateFormat;
	}

	@Override
	public TimeZone getTimeZone() {
		return timeZone;
	}

	public void setTimeZone(String timeZone) {
		this.timeZone = TimeZone.getTimeZone(timeZone);
	}

	/**
	 * Interrupts the current execution thread for a given time
	 * 
	 * @param milliseconds Number of milliseconds to wait
	 * 
	 * @throws InterruptedException Raised if any thread has interrupted the
	 *         current thread
	 */
	public void sleep(long milliseconds) throws InterruptedException {
		Thread.sleep(milliseconds);
	}
}