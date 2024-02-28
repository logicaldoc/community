package com.logicaldoc.util.time;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import org.junit.Test;

import junit.framework.TestCase;

public class JulianCalendarUtilTest extends TestCase {
	private DateFormat df = new SimpleDateFormat("yyyy-MM-dd");

	@Test
	public void testIsJulian1Date() throws ParseException {
		assertTrue(JulianCalendarUtil.isJulianDate(df.parse("1582-01-01")));
		assertFalse(JulianCalendarUtil.isJulianDate(df.parse("1582-12-01")));
	}

	@Test
	public void testToGregorian() throws ParseException {
		assertEquals(df.parse("1582-01-11"), JulianCalendarUtil.toGregorian(df.parse("1582-01-01")));
	}

	@Test
	public void testToJulian() throws ParseException {
		assertEquals(df.parse("1582-12-11"), JulianCalendarUtil.toGregorian(df.parse("1582-12-01")));
	}
}