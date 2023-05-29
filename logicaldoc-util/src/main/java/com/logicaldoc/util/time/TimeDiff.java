package com.logicaldoc.util.time;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;

public class TimeDiff {

	private TimeDiff() {
	}

	/**
	 * Calculate the absolute difference between two Date without regard for
	 * time offsets
	 * 
	 * @param d1 Date one
	 * @param d2 Date two
	 * @param field The field we're interested in out of day, hour, minute,
	 *        second, millisecond
	 * 
	 * @return The value of the required field
	 */
	public static long getTimeDifference(Date d1, Date d2, TimeField field) {
		return TimeDiff.getTimeDifference(d1, d2)[field.ordinal()];
	}

	/**
	 * Calculate the absolute difference between two dates without regard for
	 * time offsets
	 * 
	 * @param t1 Date one expressed in millisconds
	 * @param t2 Date two expressed in millisconds
	 *
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long[] getTimeDifference(long t1, long t2) {
		long[] result = new long[5];

		long diff = t2 - t1;
		final int ONE_DAY = 1000 * 60 * 60 * 24;
		final int ONE_HOUR = ONE_DAY / 24;
		final int ONE_MINUTE = ONE_HOUR / 60;
		final int ONE_SECOND = ONE_MINUTE / 60;

		long d = diff / ONE_DAY;
		diff %= ONE_DAY;

		long h = diff / ONE_HOUR;
		diff %= ONE_HOUR;

		long m = diff / ONE_MINUTE;
		diff %= ONE_MINUTE;

		long s = diff / ONE_SECOND;
		long ms = diff % ONE_SECOND;
		result[0] = d;
		result[1] = h;
		result[2] = m;
		result[3] = s;
		result[4] = ms;

		return result;
	}

	/**
	 * Calculate the absolute difference between two Date without regard for
	 * time offsets
	 * 
	 * @param d1 Date one
	 * @param d2 Date two
	 *
	 * @return The fields day, hour, minute, second and millisecond
	 */
	public static long[] getTimeDifference(Date d1, Date d2) {
		Calendar cal = Calendar.getInstance();
		cal.setTimeZone(TimeZone.getTimeZone("UTC"));
		cal.setTime(d1);
		long t1 = cal.getTimeInMillis();

		cal.setTime(d2);
		long t2 = cal.getTimeInMillis();

		return getTimeDifference(t1, t2);
	}

	public static String printDuration(Date start, Date stop) {
		return printDuration(Math.abs(start.getTime() - stop.getTime()));
	}

	/**
	 * Prints the duration in the format HH:MM:ss.SSS
	 * 
	 * @param diffMillis Duration expressed in milliseconds
	 *
	 * @return The formatted output
	 */
	public static String printDuration(long diffMillis) {
		Duration duration = Duration.of(diffMillis, ChronoUnit.MILLIS);

		long millis = duration.toMillis() % 1000;
		long minutes = duration.toMinutes() % 60;
		long seconds = (duration.toMillis() / 1000) % 60;
		long hours = duration.toMinutes() / 60;
		StringBuilder out = new StringBuilder();
		if (hours > 0) {
			out.append(StringUtils.leftPad(Long.toString(hours), 2, '0'));
			out.append(":");
		}
		if (minutes > 0 || hours > 0) {
			out.append(StringUtils.leftPad(Long.toString(minutes), 2, '0'));
			out.append(":");
		}
		out.append(StringUtils.leftPad(Long.toString(seconds), 2, '0'));
		out.append(".");
		out.append(StringUtils.leftPad(Long.toString(millis), 3, '0'));
		return out.toString();
	}

	public static enum TimeField {
		DAY, HOUR, MINUTE, SECOND, MILLISECOND;
	}
}