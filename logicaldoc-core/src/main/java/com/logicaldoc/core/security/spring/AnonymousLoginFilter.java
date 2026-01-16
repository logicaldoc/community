package com.logicaldoc.core.security.spring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class AnonymousLoginFilter extends GenericFilterBean {

	private static final Logger log = LoggerFactory.getLogger(AnonymousLoginFilter.class);

	static final String FILTER_APPLIED = "__com_logicaldoc_core_security_spring_AnonymousLoginFilter_applied";

	@Override
	public void doFilter(ServletRequest rec, ServletResponse res, FilterChain chain)
			throws IOException, ServletException {

		HttpServletRequest request = (HttpServletRequest) rec;
		HttpServletResponse response = (HttpServletResponse) res;

		if (request.getAttribute(FILTER_APPLIED) != null) {
			chain.doFilter(request, response);
			return;
		}

		request.setAttribute(FILTER_APPLIED, Boolean.TRUE);

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		if ((authentication == null || !authentication.isAuthenticated())
				&& "login".equals(request.getParameter("anonymous")))
			tryAnonymousLogin(request, response);

		chain.doFilter(request, response);
	}

	protected void tryAnonymousLogin(HttpServletRequest request, HttpServletResponse response) {
		String tenant = getTenant(request);

		ContextProperties config = Context.get().getConfig();
		if (config.getBoolean(tenant + ".anonymous.enabled", false)) {
			LDAuthenticationToken authToken = new LDAuthenticationToken(config.getProperty(tenant + ".anonymous.user"));
			AuthenticationManager authenticationManager = Context.get(AuthenticationManager.class);
			try {
				Authentication anonAuthentication = authenticationManager.authenticate(authToken);
				if (anonAuthentication.isAuthenticated()) {
					String sid = ((LDAuthenticationToken) anonAuthentication).getSid();
					SessionManager.get().saveSid(request, response, sid);

					Session session = SessionManager.get().get(sid);

					/*
					 * Save the authorities to make Spring happy
					 */
					Collection<GrantedAuthority> authorities = new ArrayList<>();
					for (String role : session.getUser().getGroups().stream().map(Group::getName).toList())
						authorities.add(new SimpleGrantedAuthority(role));

					LDAuthenticationToken auth = new LDAuthenticationToken(session.getUser(), null, authorities);
					auth.setSid(sid);

					SecurityContext sc = SecurityContextHolder.getContext();
					sc.setAuthentication(auth);
				}
			} catch (AuthenticationException ae) {
				// Noting to do
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		}
	}

	private String getTenant(HttpServletRequest request) {
		String tenant = "default";
		if (StringUtils.isNotEmpty(request.getParameter("tenant")))
			tenant = request.getParameter("tenant");
		return tenant;
	}
}