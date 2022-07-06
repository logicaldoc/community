package com.logicaldoc.webservicesamples.junit;

import junit.framework.TestCase;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.logicaldoc.webservice.soap.client.SoapAuthClient;

public abstract class BaseUnit extends TestCase {

	protected static Log log = LogFactory.getLog(BaseUnit.class);

	protected String sid;

	// LD Community Edition default address
	public static String HOST_URL = "http://localhost:8080";

	public static String AUTH_ENDPOINT = HOST_URL + "/services/Auth";

	public static String DOC_ENDPOINT = HOST_URL + "/services/Document";

	public static String FOLDER_ENDPOINT = HOST_URL + "/services/Folder";

	public static String SEARCH_ENDPOINT = HOST_URL + "/services/Search";

	public static final long DEFAULT_WORKSPACE = 4L;

	public BaseUnit() {
		super();
	}

	public BaseUnit(String name) {
		super(name);
	}

	protected void setUp() throws Exception {
		super.setUp();

		SoapAuthClient authc = new SoapAuthClient(AUTH_ENDPOINT);

		String username = "admin";
		String password = "admin";

		// Get session token
		sid = authc.login(username, password);
	}

	protected void tearDown() throws Exception {
		super.tearDown();

		SoapAuthClient authc = new SoapAuthClient(AUTH_ENDPOINT);
		authc.logout(sid);

		log.error("Logout completed successfully");
	}

}