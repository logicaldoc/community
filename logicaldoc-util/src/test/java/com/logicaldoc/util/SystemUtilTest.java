package com.logicaldoc.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

public class SystemUtilTest {

	@Test
	public void testPrintEnvironment() throws IOException {
		assertTrue(SystemUtil.printEnvironment().contains("java.version"));
	}

	@Test
	public void testPrintStackTrace() throws IOException {
		assertTrue(SystemUtil.printStackTrace().contains(SystemUtilTest.class.getName()));
	}

	@Test
	public void testGetOS() throws IOException {
		String originalOS = System.getProperty("os.name");
		try {
			System.setProperty("os.name", "Unix");
			assertEquals("uni", SystemUtil.getOS());

			System.setProperty("os.name", "unknown");
			assertEquals("err", SystemUtil.getOS());
		} finally {
			System.setProperty("os.name", originalOS);
		}
	}
}