package com.logicaldoc.util.config;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ContextConfiguratorTest {

	File contextXml = new File("target/context.xml");

	@Before
	public void setUp() throws Exception {
		FileUtil.copyResource("/context.xml", contextXml);
	}

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void testAddTrigger() {
		ContextConfigurator config = new ContextConfigurator(contextXml.getPath());
		config.addTrigger("TestTrigger");
	}
}