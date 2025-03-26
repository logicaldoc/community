package com.logicaldoc.util.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.jdom2.Element;
import org.junit.Test;

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
		assertEquals("target", config.getProperty("root"));
	}

	@Test
	public void testGetAppenders() {
		LogConfigurator config = new LogConfigurator();
		assertTrue(config.getAppenders().contains("DocuSignPoller"));
	}

	@Test
	public void testGetLoggers() {
		LogConfigurator config = new LogConfigurator();
		assertTrue(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
	}

	@Test
	public void testRemoveLogger() {
		LogConfigurator config = new LogConfigurator();
		assertTrue(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
		config.removeLogger("com.logicaldoc.core");
		assertFalse(config.getLoggers().stream().map(l -> l.getAttributeValue("name")).toList()
				.contains("com.logicaldoc.core"));
	}

	@Test
	public void testSetRootLevel() {
		LogConfigurator config = new LogConfigurator();
		assertEquals("info", config.getRootLevel());
		config.setRootLevel("debug");
		assertEquals("debug", config.getRootLevel());
	}

	@Test
	public void testGetFile() {
		LogConfigurator config = new LogConfigurator();
		String file = config.getFile("DocuSignPoller");
		assertEquals("target/docusignpoller.log", file);
	}

	@Test
	public void testSetLogsRoot() {
		LogConfigurator config = new LogConfigurator();
		config.setLogsRoot("pippo");
		assertEquals("pippo", config.getLogsRoot());
	}

	@Test
	public void testAddAppender() {
		LogConfigurator config = new LogConfigurator();
		config.addTextAppender("pippo");
		String file = config.getFile("pippo");
		assertEquals("target/pippo.log", file);

		config = new LogConfigurator();
		config.addTextAppender("DwawingsSynchronizer");
		config.addHtmlAppender("DwawingsSynchronizer_WEB");
		config.addLogger("com.logicaldoc.bazelet.DwawingsSynchronizer",
				List.of("DwawingsSynchronizer", "DwawingsSynchronizer_WEB"));

		file = config.getFile("DwawingsSynchronizer");
		assertEquals("target/dwawingssynchronizer.log", file);

		config = new LogConfigurator();
		config.addTextAppender("NLP", true, "yyyy-MM-dd");

		Element appender = config.getAppender("NLP");
		assertEquals("true", appender.getAttributeValue("immediateFlush"));
		assertEquals("yyyy-MM-dd", appender.getChild("PatternLayout").getChild("Pattern").getText());
	}

	@Test
	public void testSetLogger() {
		String loggerName = "pippo.pluto";
		LogConfigurator config = new LogConfigurator();
		assertNull(config.getLoggers().stream().filter(c -> loggerName.equals(c.getAttributeValue("name"))).findFirst()
				.orElse(null));
		config.setLogger(loggerName, false, "info", List.of("TagsProcessor"));

		Element logger = config.getLogger(loggerName);
		assertNotNull(logger);
		assertEquals("false", logger.getAttributeValue("additivity"));
		assertEquals("info", logger.getAttributeValue("level"));

		config.setLogger(loggerName, true, "warn", new ArrayList<>());
		logger = config.getLoggers().stream().filter(c -> loggerName.equals(c.getAttributeValue("name"))).findFirst()
				.orElse(null);
		assertNotNull(logger);
		assertEquals("warn", logger.getAttributeValue("level"));
		assertEquals("true", logger.getAttributeValue("additivity"));

		config.setLogger("NLP", true, "warn", List.of("DMS", "DMS_WEB"), List.of("error", "info"));
		logger = config.getLogger("NLP");
		assertNotNull(logger);
		assertEquals("error", logger.getChild("AppenderRef").getAttributeValue("level"));

	}
}