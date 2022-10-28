package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

import javax.jws.WebService;

import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.soap.AuthService;

/**
 * Auth Web Service client (SOAP).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
@WebService(name = "Auth", serviceName = "Auth")
public class SoapAuthClient extends SoapClient<AuthService> implements AuthService {

	public SoapAuthClient(String endpoint) throws IOException {
		super(endpoint, AuthService.class, -1, true, -1);
	}

	@Override
	public String login(String username, String password) throws AuthenticationException {
		return client.login(username, password);
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