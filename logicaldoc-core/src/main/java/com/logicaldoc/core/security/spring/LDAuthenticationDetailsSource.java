package com.logicaldoc.core.security.spring;

import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Our customization of an <code>AuthenticationDetailsSource</code> used to
 * extract a third authentication parameter for the 2FA.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class LDAuthenticationDetailsSource extends WebAuthenticationDetailsSource {
	@Override
	public LDAuthenticationDetails buildDetails(HttpServletRequest request) {
		return new LDAuthenticationDetails(request);
	}
}