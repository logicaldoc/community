package com.logicaldoc.util.time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;
import org.junit.Test;

import junit.framework.TestCase;

public class DateUtilTest extends TestCase {

	@Test
	public void testFormat() {
		Calendar calendar = Calendar.getInstance(Locale.getDefault());
		calendar.set(Calendar.YEAR, 1992);
		calendar.set(Calendar.MONTH, Calendar.getInstance().get(Calendar.MONTH));
		calendar.set(Calendar.DATE, 15);
		calendar.set(Calendar.HOUR_OF_DAY, 23);
		calendar.set(Calendar.MINUTE, 50);
		calendar.set(Calendar.SECOND, 13);
		calendar.set(Calendar.MILLISECOND, 27);

		DateFormat timeZoneFormat = new SimpleDateFormat("Z");

		assertEquals(
				"1992-" + StringUtils.leftPad(Integer.toString(Calendar.getInstance().get(Calendar.MONTH) + 1), 2, '0')
						+ "-15 23:50:13 " + timeZoneFormat.format(new Date()),
				DateUtil.format(calendar.getTime()));

		assertEquals(
				"1992-" + StringUtils.leftPad(Integer.toString(Calendar.getInstance().get(Calendar.MONTH) + 1), 2, '0')
						+ "-15 23:50:13.027 " + timeZoneFormat.format(new Date()),
				DateUtil.formatWithMillis(calendar.getTime()));
	}

	@Test
	public void testParse() {
		Date date = DateUtil.parse("1978-02-15 12:50:13.000 +0000");
		assertNotNull(date);

		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		calendar.set(Calendar.YEAR, 1978);
		calendar.set(Calendar.MONTH, Calendar.FEBRUARY);
		calendar.set(Calendar.DATE, 15);
		calendar.set(Calendar.HOUR_OF_DAY, 12);
		calendar.set(Calendar.MINUTE, 50);
		calendar.set(Calendar.SECOND, 13);
		calendar.set(Calendar.MILLISECOND, 0);

		assertEquals(calendar.getTime(), date);

		assertNull(DateUtil.parse("pippo"));
	}

	@Test
	public void testTruncateToDay() {
		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		calendar.set(Calendar.YEAR, 1978);
		calendar.set(Calendar.MONTH, Calendar.FEBRUARY);
		calendar.set(Calendar.DATE, 15);
		calendar.set(Calendar.HOUR_OF_DAY, 12);
		calendar.set(Calendar.MINUTE, 50);
		calendar.set(Calendar.SECOND, 13);
		calendar.set(Calendar.MILLISECOND, 0);

		Date date = DateUtil.truncateToDay(calendar.getTime());
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		assertEquals("1978-02-15 00:00:00", df.format(date));
	}
}