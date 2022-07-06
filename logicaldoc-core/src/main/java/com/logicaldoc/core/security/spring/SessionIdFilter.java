package com.logicaldoc.core.security.spring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.util.Context;

/**
 * This filter looks for a sid parameter in the request and auto-authenticate
 * the user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class SessionIdFilter extends GenericFilterBean {

	private static Logger log = LoggerFactory.getLogger(SessionIdFilter.class);

	static final String FILTER_APPLIED = "__com_logicaldoc_core_security_spring_SessionIdFilter_applied";

	public SessionIdFilter() {

	}

	@Override
	public void doFilter(ServletRequest rec, ServletResponse res, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest request = (HttpServletRequest) rec;
		HttpServletResponse response = (HttpServletResponse) res;

		if (request.getAttribute(FILTER_APPLIED) != null
				|| !Context.get().getProperties().getBoolean("security.acceptsid", false)) {
			chain.doFilter(request, response);
			return;
		}

		request.setAttribute(FILTER_APPLIED, Boolean.TRUE);

		String sid = request.getParameter(LDAuthenticationToken.PARAM_SID);

		if (StringUtils.isNotEmpty(sid)) {
			log.debug("The request specifies the sid {}", sid);

			try {
				Session session = SessionManager.get().get(sid);
				if (session == null || !session.isOpen())
					throw new AuthenticationException(String.format("Invalid session %s", sid));

				log.debug("Connecting current request to session {}", sid);

				SessionManager.get().saveSid(request, response, sid);

				// Put the cookie
				Cookie sidCookie = new Cookie(LDAuthenticationToken.COOKIE_SID, session.getSid());
				response.addCookie(sidCookie);

				// Preferably clear the password in the user object before
				// storing in authentication object
				session.getUser().clearPassword();

				String[] groups = session.getUser().getGroupNames();
				Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
				for (String role : groups)
					authorities.add(new SimpleGrantedAuthority(role));

				// Return an authenticated token, containing user data and
				// authorities
				LDAuthenticationToken a = new LDAuthenticationToken(session.getUser(), null, authorities);
				a.setSid(session.getSid());
				SecurityContextHolder.getContext().setAuthentication(a);
			} catch (AuthenticationException ae) {
				log.error(ae.getMessage(), ae);
			} catch (Throwable t) {
				log.error(t.getMessage(), t);
			}
		}

		chain.doFilter(request, response);
	}
}