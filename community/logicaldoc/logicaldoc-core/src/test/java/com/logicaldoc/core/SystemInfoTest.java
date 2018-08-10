package com.logicaldoc.core;

import java.io.UnsupportedEncodingException;

import junit.framework.Assert;

import org.junit.Test;

/**
 * Simple test to check the system infos
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class SystemInfoTest extends AbstractCoreTCase {
	@Test
	public void testGet() throws UnsupportedEncodingException {
		SystemInfo info = SystemInfo.get();
		Assert.assertEquals("LogicalDOC", info.getProduct());
		Assert.assertEquals("LogicalDOC Community", info.getProductName());
	}
}
