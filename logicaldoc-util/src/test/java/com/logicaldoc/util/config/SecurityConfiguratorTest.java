package com.logicaldoc.util.config;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>SecurityConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class SecurityConfiguratorTest {

	File contextSecurityXml = new File("target/context-security.xml");

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		FileUtil.copyResource("/context-security.xml", contextSecurityXml);
	}

	@Test
	public void testGetContentSecurityPolicy() {
		SecurityConfigurator config = new SecurityConfigurator(contextSecurityXml.getPath());
		String policies = config.getContentSecurityPolicy();
		Assert.assertNotNull(policies);
		Assert.assertTrue(policies.startsWith("default-src 'self' 'unsafe-inline' 'unsafe-eval'; script-src 'self'"));
	}
}