package com.logicaldoc.util.time;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

/**
 * Utility methods to handle dates in the Julian epoch. Based on correction
 * rules published here:
 * https://www.urbisetorbis.org/conversione-giuliana-a-gregoriana/
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class JulianCalendarUtil {

	private static List<Period> periods = new ArrayList<>();

	static {
		periods.add(new Period(-4901, -4701, -38));
		periods.add(new Period(-4701, -4601, -37));
		periods.add(new Period(-4601, -4501, -36));
		periods.add(new Period(-4501, -4301, -35));
		periods.add(new Period(-4301, -4201, -34));
		periods.add(new Period(-4201, -4101, -33));
		periods.add(new Period(-4101, -3901, -32));
		periods.add(new Period(-3901, -3801, -31));
		periods.add(new Period(-3801, -3701, -30));
		periods.add(new Period(-3701, -3501, -29));
		periods.add(new Period(-3501, -3401, -28));
		periods.add(new Period(-3401, -3301, -27));
		periods.add(new Period(-3301, -3101, -26));
		periods.add(new Period(-3101, -3001, -25));
		periods.add(new Period(-3001, -2901, -24));
		periods.add(new Period(-2901, -2701, -23));
		periods.add(new Period(-2701, -2601, -22));
		periods.add(new Period(-2601, -2501, -21));
		periods.add(new Period(-2501, -2301, -20));
		periods.add(new Period(-2301, -2301, -19));
		periods.add(new Period(-2201, -2101, -18));
		periods.add(new Period(-2101, -1901, -17));
		periods.add(new Period(-1901, -1801, -16));
		periods.add(new Period(-1801, -1701, -15));
		periods.add(new Period(-1701, -1501, -14));
		periods.add(new Period(-1501, -1401, -13));
		periods.add(new Period(-1401, -1301, -12));
		periods.add(new Period(-1301, -1101, -11));
		periods.add(new Period(-1101, -1001, -10));
		periods.add(new Period(-1001, -901, -9));
		periods.add(new Period(-901, -701, -8));
		periods.add(new Period(-701, -601, -7));
		periods.add(new Period(-601, -501, -6));
		periods.add(new Period(-501, -301, -5));
		periods.add(new Period(-301, -201, -4));
		periods.add(new Period(-201, -101, -3));
		periods.add(new Period(-101, 100, -2));
		periods.add(new Period(100, 200, -1));
		periods.add(new Period(200, 300, 0));
		periods.add(new Period(300, 500, 1));
		periods.add(new Period(500, 600, 2));
		periods.add(new Period(600, 700, 3));
		periods.add(new Period(700, 900, 4));
		periods.add(new Period(900, 1000, 5));
		periods.add(new Period(1000, 1100, 6));
		periods.add(new Period(1100, 1300, 7));
		periods.add(new Period(1300, 1400, 8));
		periods.add(new Period(1400, 1500, 9));
		periods.add(new Period(1500, 1700, 10));
		periods.add(new Period(1700, 1800, 11));
		periods.add(new Period(1800, 1900, 12));
		periods.add(new Period(1900, 2100, 13));

	}

	private JulianCalendarUtil() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Checks if a date is before the institution of the gregorian calendar on
	 * Oct 4th 1582.
	 * 
	 * @param date The date to check
	 * 
	 * @return true only if the date falls in the julian period
	 */
	public static boolean isJulianDate(Date date) {
		GregorianCalendar cal = new GregorianCalendar();
		return cal.getGregorianChange().after(date);
	}

	public static Date toGregorian(Date julianDate) {
		Period matchedPeriod = getMatchingPeriod(julianDate);

		if (matchedPeriod != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(julianDate);
			cal.add(Calendar.DATE, matchedPeriod.getCorrectionDays());
			return cal.getTime();
		} else {
			return julianDate;
		}
	}

	public static Date toJulian(Date gregorianDate) {
		Period matchedPeriod = getMatchingPeriod(gregorianDate);

		if (matchedPeriod != null) {
			Calendar cal = Calendar.getInstance();
			cal.setTime(gregorianDate);
			cal.add(Calendar.DATE, -matchedPeriod.getCorrectionDays());
			return cal.getTime();
		} else {
			return gregorianDate;
		}
	}

	private static Period getMatchingPeriod(Date date) {
		if (date == null)
			return null;

		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		String str = df.format(date);
		String[] tokens = str.split("\\-", 0);
		int year = Integer.parseInt(tokens[0]);
		int month = Integer.parseInt(tokens[1]);

		Period matchedPeriod = null;
		for (Period period : periods) {
			if (period.matches(year, month)) {
				matchedPeriod = period;
				break;
			}
		}
		return matchedPeriod;
	}

	private static class Period {
		// From March 1st this year
		private int startYear;

		// Until February 29th this year
		private int endYear;

		private int correctionDays;

		private Period(int startYear, int endYear, int correctionDays) {
			super();
			this.startYear = startYear;
			this.endYear = endYear;
			this.correctionDays = correctionDays;
		}

		public boolean matches(int year, int month) {
			if (year < startYear || year > endYear)
				return false;

			return year > startYear && year < endYear || (year == startYear && month >= 3)
					|| (year == endYear && month <= 2);
		}

		public int getCorrectionDays() {
			return correctionDays;
		}
	}
}