package com.logicaldoc.util;

import static org.junit.Assert.assertEquals;

import java.security.NoSuchAlgorithmException;

import org.junit.Test;

/**
 * Test case for <code>StringUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class StringUtilTest {
	@Test
	public void testSplit() throws NoSuchAlgorithmException {
		String str="1";
		assertEquals("1", StringUtil.split(str, '/', 3));
		str="12";
		assertEquals("12", StringUtil.split(str, '/', 3));
	    str="123";
		assertEquals("123", StringUtil.split(str, '/', 3));
		str="1234";
		assertEquals("123/4", StringUtil.split(str, '/', 3));
		str="123456";
		assertEquals("123/456", StringUtil.split(str, '/', 3));
		str="12345678";
		assertEquals("123/456/78", StringUtil.split(str, '/', 3));
	}
}