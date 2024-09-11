package com.logicaldoc.webservicesamples.junit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.webservice.soap.client.SoapAuthClient;

import junit.framework.TestCase;

public abstract class BaseTestCase extends TestCase {

	protected static Log log = LogFactory.getLog(BaseTestCase.class);

	protected String sid;

	protected SoapAuthClient authClient;

	protected ContextProperties settings;

	protected static final long DEFAULT_WORKSPACE = 4L;

	@Override
	@Before
	protected void setUp() throws Exception {
		super.setUp();

		settings = new ContextProperties();
		
		authClient = new SoapAuthClient(settings.getProperty("url") + "/services/Auth");

		// Get session token
		sid = authClient.loginApiKey(settings.getProperty("apiKey"));
	}

	@Override
	@After
	protected void tearDown() throws Exception {
		super.tearDown();

		authClient.logout(sid);
		log.error("Logout completed successfully");
	}
}