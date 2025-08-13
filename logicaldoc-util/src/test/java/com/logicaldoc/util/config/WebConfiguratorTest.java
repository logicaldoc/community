package com.logicaldoc.util.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>WebConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class WebConfiguratorTest {

	File webXml = new File("target/web.xml");

	@Before
	public void setUp() throws IOException {
		FileUtil.delete(webXml);
		FileUtil.copyResource("web.xml", webXml);
	}

	@After
	public void tearDown() {
	    FileUtil.delete(webXml);
	}

	@Test
	public void testAddServlet() {
		String notThrownTest = null;
		try {
			WebConfigurator config = new WebConfigurator(webXml.getPath());
			config.addServlet("DocumentsData", "pippo");
			notThrownTest = "ok";
		} catch (Exception t) {
			// Nothing to do
		}
		assertNotNull(notThrownTest);
	}

	@Test
	public void testSetTransportGuarantee() {
		WebConfigurator config = new WebConfigurator(webXml.getPath());
		assertTrue(config.setTransportGuarantee("CONFIDENCIAL"));
		assertFalse(config.setTransportGuarantee("CONFIDENCIAL"));

		String notThrownTest = null;
		try {
			notThrownTest = "ok";
		} catch (Exception t) {
			// Nothing to do
		}
		assertNotNull(notThrownTest);
	}
}