package com.logicaldoc.webservice.rest.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Parent for all RESTful clients
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public abstract class AbstractRestClient {

	protected static Logger log = LoggerFactory.getLogger(AbstractRestClient.class);

	protected String endpoint;

	/**
	 * Must be in the format username:password
	 */
	protected String credentials;

	protected int timeout;

	/**
	 * Constructor
	 * 
	 * @param endpoint Connection URL
	 * @param username the username
	 * @param password the password
	 */
	protected AbstractRestClient(String endpoint, String username, String password) {
		this.endpoint = endpoint;
		if (username != null && password != null) {
			this.credentials = username + ":" + password;
		}
	}

	/**
	 * Constructor
	 * 
	 * @param endpoint Connection URL
	 * @param username the suername
	 * @param password the password
	 * @param timeout Timeout for the RESTful requests
	 */
	protected AbstractRestClient(String endpoint, String username, String password, int timeout) {
		this(endpoint, username, password);
		this.timeout = timeout;
	}

}