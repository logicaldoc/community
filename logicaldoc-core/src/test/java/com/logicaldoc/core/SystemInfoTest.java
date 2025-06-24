package com.logicaldoc.core;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Simple test to check the system infos
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class SystemInfoTest extends AbstractCoreTestCase {
	@Test
	public void testGet() {
		SystemInfo info = SystemInfo.get();
		assertEquals("LogicalDOC", info.getProduct());
		assertEquals("LogicalDOC Community", info.getProductName());

		assertEquals(7, info.getMajor());
		assertEquals(7, info.getMinor());
		assertEquals(4, info.getMicro());
	}
}