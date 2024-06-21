package com.logicaldoc.util.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.IOUtil;

import junit.framework.Assert;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class ContextPropertiesTest {

	private ContextProperties contextProperties;

	private File propsFile = new File("target/testcontext.properties");

	@Before
	public void setUp() throws IOException {
		IOUtil.write(this.getClass().getResourceAsStream("/context.properties"), propsFile);
		contextProperties = new ContextProperties(propsFile);
	}

	@After
	public void trearDown() throws IOException {
		FileUtil.delete(propsFile);
		for (File backupFile : contextProperties.getBackups()) {
			FileUtil.delete(backupFile);
		}
	}

	@Test
	public void testWrite() throws IOException {
		Assert.assertEquals("target/store", contextProperties.getProperty("store.1.dir"));
		contextProperties.setProperty("test", "value");

		contextProperties.write();

		Properties properties = new Properties();
		try (InputStream is = new FileInputStream(propsFile)) {
			properties.load(is);
		}

		Assert.assertEquals("target/store", properties.getProperty("store.1.dir"));
		Assert.assertEquals("value", properties.getProperty("test"));
	}
}