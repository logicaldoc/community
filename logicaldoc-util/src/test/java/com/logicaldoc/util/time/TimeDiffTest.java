package com.logicaldoc.util.time;

import static org.junit.Assert.assertEquals;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import org.junit.Test;

import com.logicaldoc.util.time.TimeDiff.TimeField;

public class TimeDiffTest {

	private static DateFormat df() {
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
		df.setLenient(false);
		df.setTimeZone(TimeZone.getTimeZone("UTC"));
		return df;
	}

	@Test
	public void testGetTimeDifference() throws ParseException {
		DateFormat df = df();
		var date1 = df.parse("1978-02-14 12:50:13.005");
		var date2 = df.parse("1978-02-15 14:51:14.008");

		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.DAY));
		assertEquals(2L, TimeDiff.getTimeDifference(date1, date2, TimeField.HOUR));
		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.MINUTE));
		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.SECOND));
		assertEquals(3L, TimeDiff.getTimeDifference(date1, date2, TimeField.MILLISECOND));
	}

	@Test
	public void testPrintDuration() throws ParseException {
		DateFormat df = df();
		var date1 = df.parse("1978-02-14 12:50:13.005");
		var date2 = df.parse("1978-02-15 14:51:14.008");
		assertEquals("26:01:01.003", TimeDiff.printDuration(date1, date2));
	}
}