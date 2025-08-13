package com.logicaldoc.util.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>WebContextConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class WebContextConfiguratorTest {

	File metainfXml = new File("target/metainfcontext.xml");

	@Before
	public void setUp() throws IOException {
		FileUtil.delete(metainfXml);
		FileUtil.copyResource("metainfcontext.xml", metainfXml);
	}

	@After
	public void tearDown() {
		FileUtil.delete(metainfXml);
	}

	@Test
	public void testGetSameSiteCookies() {
		WebContextConfigurator config = new WebContextConfigurator(metainfXml.getPath());
		assertEquals("strict", config.getSameSiteCookies());
	}

	@Test
	public void testSetSameSiteCookies() {
		WebContextConfigurator config = new WebContextConfigurator(metainfXml.getPath());
		assertEquals("strict", config.getSameSiteCookies());
		assertTrue(config.setSameSiteCookies("lax"));
		config = new WebContextConfigurator(metainfXml.getPath());
		assertEquals("lax", config.getSameSiteCookies());
		assertFalse(config.setSameSiteCookies("lax"));
	}
}