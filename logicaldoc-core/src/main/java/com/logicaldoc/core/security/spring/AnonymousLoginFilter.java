package com.logicaldoc.core.security.spring;

import java.io.IOException;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

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
				&& "login".equals(request.getParameter("anonymous"))) {

			String tenant = getTenant(request);

			ContextProperties config = Context.get().getProperties();
			if (config.getBoolean(tenant + ".anonymous.enabled")) {
				LDAuthenticationToken authToken = new LDAuthenticationToken(
						config.getProperty(tenant + ".anonymous.user"));
				AuthenticationManager authenticationManager = Context.get(AuthenticationManager.class);
				try {
					Authentication anonAuthentication = authenticationManager.authenticate(authToken);
					if (anonAuthentication.isAuthenticated()) {
						String sid = ((LDAuthenticationToken) anonAuthentication).getSid();
						SessionManager.get().saveSid(request, response, sid);
					}
				} catch (AuthenticationException ae) {
					// Noting to do
				} catch (Exception t) {
					log.error(t.getMessage(), t);
				}
			}
		}

		chain.doFilter(request, response);
	}

	private String getTenant(HttpServletRequest request) {
		String tenant = "default";
		if (StringUtils.isNotEmpty(request.getParameter("tenant")))
			tenant = request.getParameter("tenant");
		return tenant;
	}
}