package com.logicaldoc.util.config;

import org.junit.Assert;
import org.junit.Test;

/**
 * Test case for <code>PluginDescriptorConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class PluginDescriptorConfiguratorTest {

	@Test
	public void testGetId() {
		PluginDescriptorConfigurator config = new PluginDescriptorConfigurator("src/test/resources/plgn.xml");
		Assert.assertEquals("logicaldoc-audit", config.getId());
	}

	@Test
	public void testGetVersion() {
		PluginDescriptorConfigurator config = new PluginDescriptorConfigurator("src/test/resources/plgn.xml");
		Assert.assertEquals("8.7.5", config.getVersion());
	}

	@Test
	public void testGetPluginClass() {
		PluginDescriptorConfigurator config = new PluginDescriptorConfigurator("src/test/resources/plgn.xml");
		Assert.assertEquals("com.logicaldoc.audit.AuditPlugin", config.getPluginClass());
	}

	@Test
	public void testGetDependencies() {
		PluginDescriptorConfigurator config = new PluginDescriptorConfigurator("src/test/resources/plgn.xml");
		Assert.assertEquals(2, config.getDependencies().size());
		Assert.assertTrue(config.getDependencies().contains("logicaldoc-core"));
		Assert.assertTrue(config.getDependencies().contains("logicaldoc-enterprise-core"));
	}
}