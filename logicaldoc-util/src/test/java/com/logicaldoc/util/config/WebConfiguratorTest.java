package com.logicaldoc.util.config;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

import junit.framework.Assert;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class WebConfiguratorTest {

	File webXml = new File("target/web.xml");

	@Before
	public void setUp() throws Exception {
		FileUtil.copyResource("/web.xml", webXml);
	}

	@After
	public void tearDown() throws Exception {
		// Nothing to do
	}

	@Test
	public void testAddServlet() {
		String notThrownTest = null;
		try {
			WebConfigurator config = new WebConfigurator(webXml.getPath());
			config.addServlet("DocumentsData", "pippo");
			notThrownTest = "ok";
		} catch (Throwable t) {
			// Nothing to do
		}
		Assert.assertNotNull(notThrownTest);
	}

	@Test
	public void testSetTransportGuarantee() {
		WebConfigurator config = new WebConfigurator(webXml.getPath());
		Assert.assertTrue(config.setTransportGuarantee("CONFIDENCIAL"));
		Assert.assertFalse(config.setTransportGuarantee("CONFIDENCIAL"));
		
		String notThrownTest = null;
		try {
			notThrownTest = "ok";
		} catch (Throwable t) {
			// Nothing to do
		}
		Assert.assertNotNull(notThrownTest);
	}
}