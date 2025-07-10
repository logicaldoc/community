package com.logicaldoc.util.time;

import static org.junit.Assert.assertEquals;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.util.junit.AbstractTestCase;
import com.logicaldoc.util.time.TimeDiff.TimeField;

public class TimeDiffTest extends AbstractTestCase {

	@Override
	protected List<String> getDatabaseScripts() {
		return List.of("sql1.sql");
	}

	@Test
	public void testGetTimeDifference() throws ParseException {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");

		Date date1 = df.parse("1978-02-14 12:50:13.05");
		Date date2 = df.parse("1978-02-15 14:51:14.08");

		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.DAY));
		assertEquals(2L, TimeDiff.getTimeDifference(date1, date2, TimeField.HOUR));
		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.MINUTE));
		assertEquals(1L, TimeDiff.getTimeDifference(date1, date2, TimeField.SECOND));
		assertEquals(3L, TimeDiff.getTimeDifference(date1, date2, TimeField.MILLISECOND));
	}

	@Test
	public void testPrintDuration() throws ParseException {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SS");

		Date date1 = df.parse("1978-02-14 12:50:13.05");
		Date date2 = df.parse("1978-02-15 14:51:14.08");

		assertEquals("26:01:01.003", TimeDiff.printDuration(date1, date2));
	}
}