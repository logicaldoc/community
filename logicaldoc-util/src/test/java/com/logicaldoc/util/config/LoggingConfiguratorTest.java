package com.logicaldoc.util.config;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import junit.framework.Assert;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class LoggingConfiguratorTest {

	@Before
	public void setUp() throws Exception {
		
	}

	@After
	public void tearDown() throws Exception {

	}
	
	@Test
	public void testGetProperty() {
		LoggingConfigurator config = new LoggingConfigurator();
		Assert.assertEquals("target", config.getProperty("root"));
	}

	@Test
	public void testGetLoggingFiles() {
		LoggingConfigurator config = new LoggingConfigurator();
		Assert.assertTrue(config.getLoggingFiles().contains("DocuSignPoller"));
	}

	@Test
	public void testGetFile() {
		LoggingConfigurator config = new LoggingConfigurator();
		String file = config.getFile("DocuSignPoller");
		Assert.assertEquals("target/docusignpoller.log", file);
	}

	@Test
	public void testSetLogsRoot() {
		LoggingConfigurator config = new LoggingConfigurator();
		config.setLogsRoot("pippo");
		Assert.assertEquals("pippo", config.getLogsRoot());
	}
	
	@Test
	public void testAddTextAppender() {
		LoggingConfigurator config = new LoggingConfigurator();
		config.addTextAppender("pippo");
		String file = config.getFile("pippo");
		Assert.assertEquals("target/pippo.log", file);
		
		config = new LoggingConfigurator();
		config.addTextAppender("DwawingsSynchronizer");
		config.write();
		config.addHtmlAppender("DwawingsSynchronizer_WEB");
		config.write();
		config.addLogger("com.logicaldoc.bazelet.DwawingsSynchronizer", new String[] { "DwawingsSynchronizer", "DwawingsSynchronizer_WEB" });
		config.write();
		
		file = config.getFile("DwawingsSynchronizer");
		Assert.assertEquals("target/dwawingssynchronizer.log", file);
	}
}