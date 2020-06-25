package com.logicaldoc.core.security.spring;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;

import com.logicaldoc.core.security.authentication.AccountNotFoundException;
import com.logicaldoc.core.security.authentication.WrongPasswordException;

/**
 * This handler gets the j_failureurl request parameter and use it's value to
 * redirect the user after a successful login.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationFailureHandler implements AuthenticationFailureHandler {

	private static Logger log = LoggerFactory.getLogger(LDAuthenticationFailureHandler.class);

	private static final String COOKIE_LDOC_FAILURE = "ldoc-failure";

	private static final String PARAM_FAILUREURL = "j_failureurl";

	@Override
	public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
			AuthenticationException exception) throws IOException, ServletException {
		StringBuffer failureUrl = new StringBuffer(request.getContextPath());
		failureUrl.append("/");
		if (request.getParameter(PARAM_FAILUREURL) != null)
			failureUrl.append(request.getParameter(PARAM_FAILUREURL));
		if (failureUrl.toString().indexOf('?') != -1)
			failureUrl.append("&");
		else
			failureUrl.append("?");
		failureUrl.append("failure=");

		String failureReason = exception.getMessage();

		/*
		 * Mask the not found as a wrong password, in order to avoid
		 * vulnerability "Username Enumeration Weakness"<br>
		 * https://www.zeroscience.mk/en/vulnerabilities/ZSL-2018-5451.php
		 */
		if (AccountNotFoundException.CODE.equals(failureReason))
			failureReason = WrongPasswordException.CODE;
		failureUrl.append(failureReason);

		try {
			Cookie failureCookie = new Cookie(COOKIE_LDOC_FAILURE, failureReason);
			failureCookie.setMaxAge(10);
			response.addCookie(failureCookie);
		} catch (Throwable t) {

		}

		String username = request.getParameter("j_username");
		log.warn("Authentication of {} was unsuccesful due to {}", username, failureReason);

		String normalizedFailureUrl = failureUrl.toString().replace("//", "/");
		if (normalizedFailureUrl.startsWith("/"))
			normalizedFailureUrl = normalizedFailureUrl.substring(1);

		log.info("Redirecting to {}", normalizedFailureUrl.toString());
		response.sendRedirect(failureUrl.toString());
	}
}