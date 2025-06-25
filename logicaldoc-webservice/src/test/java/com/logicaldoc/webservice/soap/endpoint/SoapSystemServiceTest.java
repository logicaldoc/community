package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;

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
		assertEquals("via Aldo Moro interna, 3", info.getVendorAddress());
		assertEquals("1234567890", info.getInstallationId());
		assertEquals("6.1 Beta2", info.getRelease());
		assertEquals("2006-2021", info.getYear());
	}

	@Test
	public void testGetStatistics() throws Exception {
		List<WSParameter> parameters = systemServiceImpl.getStatistics("");

		Map<String, String> map = parameters.stream().collect(Collectors.toMap(p -> p.getName(), p -> p.getValue()));

		assertEquals("5437281", map.get("repo_storage"));
		assertEquals("986753", map.get("repo_database"));
		assertEquals("181", map.get("docs_indexed"));
		assertEquals("45", map.get("folder_withdocs"));
		assertEquals("2011-02-15 10:46:27", map.get("stats_lastrun"));
		assertEquals("0", map.get("repo_import"));
	}
}