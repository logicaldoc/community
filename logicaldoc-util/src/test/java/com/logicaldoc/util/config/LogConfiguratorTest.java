package com.logicaldoc.util.config;

import java.util.ArrayList;
import java.util.List;

import org.jdom2.Element;
import org.junit.Test;

import junit.framework.Assert;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class LogConfiguratorTest {

	@Test
	public void testGetProperty() {
		LogConfigurator config = new LogConfigurator();
		Assert.assertEquals("target", config.getProperty("root"));
	}

	@Test
	public void testGetAppenders() {
		LogConfigurator config = new LogConfigurator();
		Assert.assertTrue(config.getAppenders().contains("DocuSignPoller"));
	}

	@Test
	public void testGetLoggers() {
		LogConfigurator config = new LogConfigurator();
		Assert.assertTrue(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
	}

	@Test
	public void testRemoveLogger() {
		LogConfigurator config = new LogConfigurator();
		Assert.assertTrue(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
		config.removeLogger("com.logicaldoc.core");
		Assert.assertFalse(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
	}

	@Test
	public void testSetRootLevel() {
		LogConfigurator config = new LogConfigurator();
		Assert.assertEquals("info", config.getRootLevel());
		config.setRootLevel("debug");
		Assert.assertEquals("debug", config.getRootLevel());
	}

	@Test
	public void testGetFile() {
		LogConfigurator config = new LogConfigurator();
		String file = config.getFile("DocuSignPoller");
		Assert.assertEquals("target/docusignpoller.log", file);
	}

	@Test
	public void testSetLogsRoot() {
		LogConfigurator config = new LogConfigurator();
		config.setLogsRoot("pippo");
		Assert.assertEquals("pippo", config.getLogsRoot());
	}

	@Test
	public void testAddAppender() {
		LogConfigurator config = new LogConfigurator();
		config.addTextAppender("pippo");
		String file = config.getFile("pippo");
		Assert.assertEquals("target/pippo.log", file);

		config = new LogConfigurator();
		config.addTextAppender("DwawingsSynchronizer");
		config.addHtmlAppender("DwawingsSynchronizer_WEB");
		config.addLogger("com.logicaldoc.bazelet.DwawingsSynchronizer",
				List.of("DwawingsSynchronizer", "DwawingsSynchronizer_WEB"));

		file = config.getFile("DwawingsSynchronizer");
		Assert.assertEquals("target/dwawingssynchronizer.log", file);
	}

	@Test
	public void testSetLogger() {
		String loggerName = "pippo.pluto";
		LogConfigurator config = new LogConfigurator();
		Assert.assertNull(config.getLoggers().stream().filter(c -> loggerName.equals(c.getAttributeValue("name")))
				.findFirst().orElse(null));
		config.setLogger(loggerName, false, "info", List.of("TagsProcessor"));

		Element logger = config.getLoggers().stream().filter(c -> loggerName.equals(c.getAttributeValue("name")))
				.findFirst().orElse(null);
		Assert.assertNotNull(logger);
		Assert.assertEquals("false", logger.getAttributeValue("additivity"));
		Assert.assertEquals("info", logger.getAttributeValue("level"));

		config.setLogger(loggerName, true, "warn", new ArrayList<>());
		logger = config.getLoggers().stream().filter(c -> loggerName.equals(c.getAttributeValue("name"))).findFirst()
				.orElse(null);
		Assert.assertNotNull(logger);
		Assert.assertEquals("warn", logger.getAttributeValue("level"));
		Assert.assertEquals("true", logger.getAttributeValue("additivity"));
	}
}