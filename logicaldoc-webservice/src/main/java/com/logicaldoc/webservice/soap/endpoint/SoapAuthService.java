package com.logicaldoc.webservice.soap.endpoint;

import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.soap.AuthService;

/**
 * Auth Web Service Implementation (SOAP)
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapAuthService extends AbstractService implements AuthService {

	@Override
	public String login(String username, String password) throws AuthenticationException {
		if (!isWebserviceEnabled())
			throw new AuthenticationException("Webservices are disabled");
		if (Context.get().getConfig().getBoolean("webservice.basicauth.enabled", false))
			return SessionManager.get().newSession(username, password, getCurrentRequest()).getSid();
		else
			throw new AuthenticationException("Basic Authentication is disabled");
	}

	@Override
	public String loginApiKey(String apiKey) throws AuthenticationException {
		if (!isWebserviceEnabled())
			throw new AuthenticationException("Webservices are disabled");
		return SessionManager.get().newSession(apiKey, getCurrentRequest()).getSid();
	}

	@Override
	public void logout(String sidOrApiKey) {
		if (isWebserviceEnabled())
			SessionManager.get().kill(sessionId(sidOrApiKey));
	}

	@Override
	public boolean valid(String sidOrApiKey) {
		if (!isWebserviceEnabled())
			return false;
		return SessionManager.get().isOpen(sessionId(sidOrApiKey));
	}

	@Override
	public void renew(String sidOrApiKey) {
		if (!isWebserviceEnabled())
			return;
		String sid = sessionId(sidOrApiKey);
		if (SessionManager.get().isOpen(sid))
			SessionManager.get().renew(sid);
	}
}