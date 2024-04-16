package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

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
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
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

		Assert.assertEquals("5437281", parameters.get(0).getValue());
		Assert.assertEquals("986753", parameters.get(6).getValue());
		Assert.assertEquals("181", parameters.get(9).getValue());
		Assert.assertEquals("45", parameters.get(11).getValue());
		Assert.assertEquals("2011-02-15 10:46:27", parameters.get(14).getValue());
		Assert.assertEquals("0", parameters.get(4).getValue());
	}
}
