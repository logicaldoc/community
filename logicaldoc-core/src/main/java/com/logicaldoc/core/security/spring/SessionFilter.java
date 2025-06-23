package com.logicaldoc.core.security.spring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.user.Group;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This filter looks for a sid parameter in the request or X-API-KEY header and
 * auto-authenticate the user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class SessionFilter extends GenericFilterBean {

	private static final Logger log = LoggerFactory.getLogger(SessionFilter.class);

	static final String FILTER_APPLIED = "__com_logicaldoc_core_security_spring_SessionFilter_applied";

	@Override
	public void doFilter(ServletRequest rec, ServletResponse res, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) rec;
		HttpServletResponse response = (HttpServletResponse) res;

		// Do not execute the filter twice
		if (request.getAttribute(FILTER_APPLIED) != null) {
			chain.doFilter(request, response);
			return;
		}

		request.setAttribute(FILTER_APPLIED, Boolean.TRUE);

		// Do not execute the filter if there is an existing authentication
		// already
		if (SecurityContextHolder.getContext().getAuthentication() != null && request.getSession(false) != null
				&& request.getSession(false).getAttribute("sid") != null) {
			chain.doFilter(request, response);
			return;
		}

		String sid = getSidFromRequest(request);

		if (StringUtils.isNotEmpty(sid)) {
			try {
				Session session = SessionManager.get().get(sid);
				if (session == null || !session.isOpen()) {
					log.debug("The sid {} is unexisting or expired", sid);
					chain.doFilter(request, response);
					return;
				}
				log.debug("Connecting current request to session {}", sid);

				SessionManager.get().saveSid(request, response, sid);

				// Preferably clear the password in the user object before
				// storing in authentication object
				session.getUser().clearPassword();

				Collection<GrantedAuthority> authorities = new ArrayList<>();
				for (String role : session.getUser().getGroups().stream().map(Group::getName).toList())
					authorities.add(new SimpleGrantedAuthority(role));

				// Return an authenticated token, containing user data and
				// authorities
				LDAuthenticationToken auth = new LDAuthenticationToken(session.getUser(), null, authorities);
				auth.setSid(session.getSid());

				SecurityContext sc = SecurityContextHolder.getContext();
				sc.setAuthentication(auth);
			} catch (AuthenticationException ae) {
				log.error(ae.getMessage(), ae);
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		}

		chain.doFilter(request, response);
	}

	private String getSidFromRequest(HttpServletRequest request) {
		String sid = SessionManager.get().getSessionId(request);
		if (StringUtils.isEmpty(sid) && StringUtils.isNotEmpty(request.getHeader(SessionManager.HEADER_APIKEY))) {
			try {
				Session session = SessionManager.get().newSession(null, null,
						request.getHeader(SessionManager.HEADER_APIKEY), request);
				sid = session.getSid();
			} catch (AuthenticationException ae) {
				log.error(ae.getMessage(), ae);
			}
		}
		return sid;
	}
}