package com.logicaldoc.webservice.rest.client;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webservice.rest.AuthService;

/**
 * Auth Web Service client (RESTful).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @author Alessandro Gasparini - LogicalDOC
 * @since 6.9
 */
public class RestAuthClient extends AbstractRestClient implements AuthService {

	protected static Logger log = LoggerFactory.getLogger(RestAuthClient.class);

	private AuthService proxy = null;

	public RestAuthClient(String endpoint) {
		super(endpoint, null, null);
		proxy = JAXRSClientFactory.create(endpoint, AuthService.class);
	}

	public String login(String username, String password) throws Exception {
		return proxy.login(username, password);
	}

	public void logout(String sid) {
		proxy.logout(sid);
	}

	@Override
	public String loginPost(String username, String password) throws Exception {
		return proxy.loginPost(username, password);
	}

	@Override
	public String loginPostJSON(String jsonstr) throws Exception {
		return proxy.loginPostJSON(jsonstr);
	}
	
	@Override
	public String getSid() {
		return proxy.getSid();
	}	
}