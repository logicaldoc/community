package com.logicaldoc.core.security.spring;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;

/**
 * This handler gets the j_failureurl request parameter and use it's value to
 * redirect the user after a successful login.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationFailureHandler implements AuthenticationFailureHandler {

	private static Logger log = LoggerFactory.getLogger(LDAuthenticationFailureHandler.class);

	private static final String PARAM_FAILUREURL = "j_failureurl";

	@Override
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
			AuthenticationException exception) throws IOException, ServletException {
		StringBuffer failureUrl = new StringBuffer(request.getContextPath());
		failureUrl.append("/");
		if (request.getParameter(PARAM_FAILUREURL) != null)
			failureUrl.append(request.getParameter(PARAM_FAILUREURL));

		String failureReason = exception.getMessage();

		String username = request.getParameter("j_username");
		log.warn("Authentication of {} was unsuccesful due to {}", username, failureReason);

		String normalizedFailureUrl = failureUrl.toString().replace("//", "/");
		if (normalizedFailureUrl.startsWith("/"))
			normalizedFailureUrl = normalizedFailureUrl.substring(1);

		log.info("Redirecting to {}", normalizedFailureUrl.toString());
		response.sendRedirect(failureUrl.toString());
	}
}