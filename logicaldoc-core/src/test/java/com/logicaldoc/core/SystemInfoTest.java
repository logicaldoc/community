package com.logicaldoc.core;

import org.junit.Test;

import junit.framework.Assert;

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
		Assert.assertEquals("LogicalDOC", info.getProduct());
		Assert.assertEquals("LogicalDOC Community", info.getProductName());

		Assert.assertEquals(7, info.getMajor());
		Assert.assertEquals(7, info.getMinor());
		Assert.assertEquals(4, info.getMicro());
	}
}
