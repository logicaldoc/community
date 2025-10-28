package com.logicaldoc.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import org.junit.Test;

/**
 * Test case for <code>StringUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class StringUtilTest {
	@Test
	public void testSplit() {
		String str = "1";
		assertEquals("1", StringUtil.split(str, '/', 3));
		str = "12";
		assertEquals("12", StringUtil.split(str, '/', 3));
		str = "123";
		assertEquals("123", StringUtil.split(str, '/', 3));
		str = "1234";
		assertEquals("123/4", StringUtil.split(str, '/', 3));
		str = "123456";
		assertEquals("123/456", StringUtil.split(str, '/', 3));
		str = "12345678";
		assertEquals("123/456/78", StringUtil.split(str, '/', 3));
	}

	@Test
	public void testWriteToString() throws Exception {
		String expected = "hello world" + System.lineSeparator();
		String result = StringUtil.writeToString(new StringReader(expected));
		assertEquals(expected, result);
	}

	@Test
	public void testWriteToStringUtf8() throws Exception {
		String input = "Hello";
		InputStream is = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8));

		String result = StringUtil.writeToString(is, "UTF-8");

		assertEquals(input, result);
	}

	@Test
	public void testArrayToString() {
		assertEquals("A, B, C", StringUtil.arrayToString(new Object[] { "A", "B", "C" }, ", "));
	}

	@Test
	public void testCollectionToString() {
		List<String> list = Arrays.asList("A", "B", "C");
		assertEquals("A, B, C", StringUtil.collectionToString(list, ", "));
	}

	@Test
	public void testRemoveNonUtf8Chars() {
		assertEquals("Hello World", StringUtil.removeNonUtf8Chars("Hello World"));
	}

	@Test
	public void testMatches() {
		assertTrue(StringUtil.matches("alpha", null, null));
		assertTrue(StringUtil.matches("alpha", new String[] { "alpha" }, null));
		assertFalse(StringUtil.matches("alpha", null, new String[] { "alpha" }));
		assertFalse(StringUtil.matches("alpha", new String[] { "alpha" }, new String[] { "alpha" }));
		assertTrue(StringUtil.matches("alpha", new String[] {}, new String[] { "beta" }));
		assertFalse(StringUtil.matches("alpha", new String[] { "beta", "gamma" }, null));
	}

	@Test
	public void testUnaccent() {
		assertEquals("Cafe latte", StringUtil.unaccent("Caf\u00E9 latt\u00E9"));
	}

	@Test
	public void testPrintFileSize() {
		assertEquals(StringUtil.printFileSize(0, Locale.getDefault()), StringUtil.printFileSize(0));
		assertEquals(StringUtil.printFileSize(512, Locale.getDefault()), StringUtil.printFileSize(512));
		assertEquals(StringUtil.printFileSize(1024L, Locale.getDefault()), StringUtil.printFileSize(1024L));
	}
	
	@Test
	public void testDefaultString() {
		assertEquals("default", StringUtil.defaultString(null, "default"));
	}
	
	@Test
	public void testFirstWords() {
		assertEquals("pippo pluto", StringUtil.firstWords("pippo pluto paperino", 2));
		assertEquals("pippo pluto paperino", StringUtil.firstWords("pippo pluto paperino", 3));
		assertEquals("pippo pluto paperino", StringUtil.firstWords("pippo pluto paperino", 10));
		assertEquals("pippo", StringUtil.firstWords("pippo pluto paperino", 1));
		assertEquals("", StringUtil.firstWords("", 1));
	}
}