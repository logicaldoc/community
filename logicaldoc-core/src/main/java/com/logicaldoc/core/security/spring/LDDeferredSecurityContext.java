package com.logicaldoc.core.security.spring;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.context.DeferredSecurityContext;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

public class LDDeferredSecurityContext implements DeferredSecurityContext {

	static Map<String, HttpSession> servletSessionMapping = new HashMap<>();

	private HttpServletRequest request;

	private SecurityContext context;

	public LDDeferredSecurityContext(HttpServletRequest request) {
		this.request = request;
	}

	@Override
	public SecurityContext get() {
		if (isGenerated())
			return context;

		String sid = SessionManager.get().getSessionId(request);

		if (sid == null || !SessionManager.get().isOpen(sid)) {
			context = SecurityContextHolder.createEmptyContext();
		} else {
			Session session = SessionManager.get().get(sid);

			String username = session.getClient() != null && StringUtils.isNotEmpty(session.getClient().getUsername())
					? session.getClient().getUsername()
					: session.getUsername();

			LDAuthenticationToken token = new LDAuthenticationToken(username, "", null);
			token.setSid(sid);

			context = new SecurityContextImpl();
			context.setAuthentication(token);

			HttpSession servletSession = request.getSession(false);
			if (servletSession != null)
				servletSessionMapping.put(sid, servletSession);
		}
		return context;
	}

	@Override
	public boolean isGenerated() {
		return context != null;
	}

	public static void bindServletSession(String sid, HttpServletRequest request) {
		bindServletSession(sid, request.getSession());
	}

	public static void bindServletSession(String sid, HttpSession servletSession) {
		servletSession.setAttribute(SessionManager.PARAM_SID, sid);
		servletSessionMapping.put(sid, servletSession);
	}

	public static HttpSession getServletSession(String sid) {
		return servletSessionMapping.get(sid);
	}
}
