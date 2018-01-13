package com.logicaldoc.util.config;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - Logical Objects
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
	public void testAddTrigger() {
		LoggingConfigurator config = new LoggingConfigurator();
		String file = config.getFile("DMS");
		Assert.assertEquals("D:/LogicalDOC-Devel/repository/logs/dms.log", file);
	}
}