package com.logicaldoc.core.security.spring;

import org.springframework.security.core.context.DeferredSecurityContext;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

import com.logicaldoc.core.security.SessionManager;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * This makes use of sessions nut also uses our request-sid binding.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDSecurityContextRepository extends HttpSessionSecurityContextRepository {

	@Override
	public boolean containsContext(HttpServletRequest request) {
		return super.containsContext(request) || SessionManager.get().getSessionId(request) != null;
	}

	@Override
	public DeferredSecurityContext loadDeferredContext(HttpServletRequest request) {
		DeferredSecurityContext context = super.loadDeferredContext(request);
		return context != null ? context : new LDDeferredSecurityContext(request);
	}

	@Override
	public void saveContext(SecurityContext context, HttpServletRequest request, HttpServletResponse response) {
		super.saveContext(context, request, response);
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