package com.logicaldoc.util.config;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

import junit.framework.Assert;

/**
 * Test case for <code>WebContextConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class WebContextConfiguratorTest {

	File webXml = new File("target/metainfcontext.xml");

	@Before
	public void setUp() throws Exception {
		FileUtil.copyResource("/metainfcontext.xml", webXml);
	}

	@After
	public void tearDown() throws Exception {
		// Nothing to do
	}

	@Test
	public void testGetSameSiteCookies() {
		WebContextConfigurator config = new WebContextConfigurator(webXml.getPath());
		Assert.assertEquals("strict", config.getSameSiteCookies());
	}
	
	@Test
	public void testSetSameSiteCookies() {
		WebContextConfigurator config = new WebContextConfigurator(webXml.getPath());
		Assert.assertEquals("strict", config.getSameSiteCookies());
		Assert.assertTrue(config.setSameSiteCookies("lax"));
		config = new WebContextConfigurator(webXml.getPath());
		Assert.assertEquals("lax", config.getSameSiteCookies());
		Assert.assertFalse(config.setSameSiteCookies("lax"));
	}
}