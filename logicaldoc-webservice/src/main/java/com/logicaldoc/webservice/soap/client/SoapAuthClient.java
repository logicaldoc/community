package com.logicaldoc.webservice.soap.client;

import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.soap.AuthService;

import jakarta.jws.WebService;

/**
 * Auth Web Service client (SOAP).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@WebService(name = "Auth", serviceName = "Auth")
public class SoapAuthClient extends SoapClient<AuthService> implements AuthService {

	public SoapAuthClient(String endpoint) {
		super(endpoint, AuthService.class, -1, true, -1);
	}

	@Override
	public String login(String username, String password) throws AuthenticationException {
		return client.login(username, password);
	}
	
	@Override
	public String loginApiKey(String apiKey) throws AuthenticationException {
		return client.loginApiKey(apiKey);
	}

	@Override
	public void logout(String sid) {
		client.logout(sid);
	}

	@Override
	public boolean valid(String sid) {
		return client.valid(sid);
	}

	@Override
	public void renew(String sid) {
		client.renew(sid);
	}
}