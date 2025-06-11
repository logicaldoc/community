package com.logicaldoc.core.security.spring;

import org.springframework.security.core.context.DeferredSecurityContext;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

import com.logicaldoc.core.security.SessionManager;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * This repository avoid the use of sessions and simply use the current request
 * to store and retrieve the session ID.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDSecurityContextRepository extends HttpSessionSecurityContextRepository {

	@Override
	public boolean containsContext(HttpServletRequest request) {
		return SessionManager.get().getSessionId(request) != null;
	}

	@Override
	public DeferredSecurityContext loadDeferredContext(HttpServletRequest request) {
		return new LDDeferredSecurityContext(request);
	}

	@Override
	public void saveContext(SecurityContext context, HttpServletRequest request, HttpServletResponse response) {
		if (context.getAuthentication() == null)
			return;

		Object principal = context.getAuthentication().getPrincipal();

		if (principal instanceof LDAuthenticationToken token) {
			SessionManager.get().saveSid(request, response, token.getSid());

			HttpSession servletSession = request.getSession(false);

			if (servletSession != null && servletSession.getAttribute(SessionManager.PARAM_SID) != null)
				LDDeferredSecurityContext.bindServletSession(
						servletSession.getAttribute(SessionManager.PARAM_SID).toString(), servletSession);
		}
	}
}