package com.logicaldoc.webservice.rest.client;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.model.WSCredentials;
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

	public RestAuthClient(String endpoint) throws AuthenticationException {
		super(endpoint, null, null);
		proxy = JAXRSClientFactory.create(endpoint, AuthService.class);
	}

	public String login(String username, String password) {
		return proxy.login(username, password);
	}

	public void logout(String sid) {
		proxy.logout(sid);
	}

	@Override
	public String loginPostJSON(WSCredentials wscred) {
		return proxy.loginPostJSON(wscred);
	}

	@Override
	public String getSid() {
		return proxy.getSid();
	}

	@Override
	public String loginForm(String username, String password) {
		return proxy.loginForm(username, password);
	}
	
	@Override
	public String loginApiKey(String apiKey) {
		return proxy.loginApiKey(apiKey);
	}
}