package com.logicaldoc.util.config;

import java.io.File;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - Logical Objects
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

	}

	@Test
	public void testAddServlet() {
		WebConfigurator config = new WebConfigurator(webXml.getPath());
		config.addServlet("DocumentsData", "pippo");
	}

	@Test
	public void testSetTransportGuarantee() {
		WebConfigurator config = new WebConfigurator(webXml.getPath());
		Assert.assertTrue(config.setTransportGuarantee("CONFIDENCIAL"));
		Assert.assertFalse(config.setTransportGuarantee("CONFIDENCIAL"));
	}
}