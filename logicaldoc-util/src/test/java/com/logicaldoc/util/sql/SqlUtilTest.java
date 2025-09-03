package com.logicaldoc.util.sql;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Test case for <code>SqlUtil</code>
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since <9.2.1>
 */
public class SqlUtilTest {

	@Test
	public void testDoubleQuotes() {
		assertEquals("", SqlUtil.doubleQuotes(null));
		assertEquals("Hello World", SqlUtil.doubleQuotes("Hello World"));
		assertEquals("It''s a test: ''quote''", SqlUtil.doubleQuotes("It's a test: 'quote'"));
	}

	@Test
	public void testDoubleBackslahes() {
		assertEquals("", SqlUtil.doubleBackslashes(""));
		assertEquals("", SqlUtil.doubleBackslashes(null));
	}

	@Test
	public void testQuotesDoubleBackslahes() {
		assertEquals("C:\\\\path\\\\file", SqlUtil.doubleQuotesAndBackslashes("C:\\path\\file"));
	}
}
