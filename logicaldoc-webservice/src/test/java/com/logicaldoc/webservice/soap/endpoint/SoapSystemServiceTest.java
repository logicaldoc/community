package com.logicaldoc.webservice.soap.endpoint;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Test;

import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;

import junit.framework.Assert;

/**
 * Test case for <code>SoapSystemService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapSystemServiceTest extends AbstractWebserviceTestCase {
	// Instance under test
	private SoapSystemService systemServiceImpl;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Make sure that this is a SoapSystemService instance
		systemServiceImpl = new SoapSystemService();
		systemServiceImpl.setValidateSession(false);
	}

	@Test
	public void testGetInfo() throws Exception {
		WSSystemInfo info = systemServiceImpl.getInfo();
		Assert.assertEquals("via Aldo Moro interna, 3", info.getVendorAddress());
		Assert.assertEquals("1234567890", info.getInstallationId());
		Assert.assertEquals("6.1 Beta2", info.getRelease());
		Assert.assertEquals("2006-2021", info.getYear());
	}

	@Test
	public void testGetStatistics() throws Exception {
		List<WSParameter> parameters = systemServiceImpl.getStatistics("");

		Map<String, String> map = parameters.stream().collect(Collectors.toMap(p -> p.getName(), p -> p.getValue()));
		
		Assert.assertEquals("5437281", map.get("repo_storage"));
		Assert.assertEquals("986753", map.get("repo_database"));
		Assert.assertEquals("181", map.get("docs_indexed"));
		Assert.assertEquals("45", map.get("folder_withdocs"));
		Assert.assertEquals("2011-02-15 10:46:27", map.get("stats_lastrun"));
		Assert.assertEquals("0", map.get("repo_import"));
	}
}