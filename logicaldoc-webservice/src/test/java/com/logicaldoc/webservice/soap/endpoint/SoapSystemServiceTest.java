package com.logicaldoc.webservice.soap.endpoint;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Test;

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
	public void setUp() throws FileNotFoundException, IOException, SQLException {
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
		WSParameter[] parameters = systemServiceImpl.getStatistics("");

		Assert.assertEquals("5437281", parameters[0].getValue());
		Assert.assertEquals("986753", parameters[6].getValue());
		Assert.assertEquals("181", parameters[9].getValue());
		Assert.assertEquals("45", parameters[11].getValue());
		Assert.assertEquals("2011-02-15 10:46:27", parameters[14].getValue());
		Assert.assertEquals("0", parameters[4].getValue());
	}
}
