package com.logicaldoc.util;

import static org.junit.Assert.*;

import java.security.NoSuchAlgorithmException;

import org.junit.Test;

import com.logicaldoc.util.crypt.CryptUtil;

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
		
		System.out.println("Crypt J2A86bc26QnIpf1:"+CryptUtil.encryptSHA256("J2A86bc26QnIpf1"));
	}
}