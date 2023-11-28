package com.logicaldoc.webservice.soap.endpoint;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.soap.AuthService;

/**
 * Auth Web Service Implementation (SOAP)
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
public class SoapAuthService extends AbstractService implements AuthService {
	protected static Logger log = LoggerFactory.getLogger(SoapAuthService.class);

	@Override
	public String login(String username, String password) throws AuthenticationException {
		HttpServletRequest request = getCurrentRequest();

		Session session = SessionManager.get().newSession(username, password,
				SessionManager.get().buildClient(request));
		return session.getSid();
	}

	@Override
	public void logout(String sid) {
		SessionManager.get().kill(sid);
	}

	@Override
	public boolean valid(String sid) {
		if (!isWebserviceEnabled())
			return false;
		return SessionManager.get().isOpen(sid);
	}

	@Override
	public void renew(String sid) {
		if (!isWebserviceEnabled())
			return;
		if (SessionManager.get().isOpen(sid))
			SessionManager.get().renew(sid);
	}
}