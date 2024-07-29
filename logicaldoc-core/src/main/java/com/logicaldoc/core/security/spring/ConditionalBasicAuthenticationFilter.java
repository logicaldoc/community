package com.logicaldoc.core.security.spring;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * An filter for the basic authentication that is enabled or not depending on a
 * boolean configuration parameter.
 *
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4 *
 */
public class ConditionalBasicAuthenticationFilter extends BasicAuthenticationFilter {

	private String enableParam = "webservice.basicauth.enabled";

	public ConditionalBasicAuthenticationFilter(AuthenticationManager authenticationManager) {
		super(authenticationManager);
	}

	public ConditionalBasicAuthenticationFilter(AuthenticationManager authenticationManager,
			AuthenticationEntryPoint authenticationEntryPoint) {
		super(authenticationManager, authenticationEntryPoint);
	}

	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		if (Context.get().getProperties().getBoolean(enableParam, false)
				|| SessionManager.get().getSessionId(request) != null)
			super.doFilterInternal(request, response, chain);
		else
			throw new ServletException("Basic Authentication is disabled");
	}

	public void setEnableParam(String enableParam) {
		this.enableParam = enableParam;
	}
}