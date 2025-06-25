package com.logicaldoc.util.config;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;

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
	public void setUp() throws IOException {
		FileUtil.copyResource("contexttest.xml", contextXml);
	}

	@Test
	public void testAddTrigger() {
		String notThrownTest = null;
		try {
			ContextConfigurator config = new ContextConfigurator(contextXml.getPath());
			config.addTrigger("TestTrigger");
			notThrownTest = "ok";
		} catch (Exception t) {
			// Ignore
		}
		assertNotNull(notThrownTest);
	}
}