package com.logicaldoc.core.security.spring;

import java.io.IOException;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.savedrequest.HttpSessionRequestCache;
import org.springframework.security.web.savedrequest.RequestCache;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * This handler gets the j_successurl request parameter and use it's value to
 * redirect the user after a successful login.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationSuccessHandler extends SavedRequestAwareAuthenticationSuccessHandler {
	private static final Logger log = LoggerFactory.getLogger(LDAuthenticationSuccessHandler.class);

	private static final String PARAM_SUCCESSURL = "j_successurl";

	private static RequestCache myCache = new HttpSessionRequestCache();

	public LDAuthenticationSuccessHandler() {
		setRequestCache(myCache);
	}

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws IOException, ServletException {

		/*
		 * Deduct the destination URL
		 */
		String requestedUrl = request.getParameter(PARAM_SUCCESSURL);
		if (requestedUrl == null && request.getSession(false) != null)
			request.getSession(false).removeAttribute(LDAccessDecisionManager.REQUESTED_URL);

		LDAuthenticationToken token = (LDAuthenticationToken) authentication;

		Cookie sidCookie = new Cookie(LDAuthenticationToken.COOKIE_SID, token.getSid());
		sidCookie.setHttpOnly(true);
		sidCookie.setSecure(Context.get().getProperties().getBoolean("cookies.secure", false));
		sidCookie.setPath("/");
		response.addCookie(sidCookie);
		response.setHeader("SID", token.getSid());
		
        log.debug("authentication success {} with sid {}", requestedUrl, token.getSid());		
		SessionManager.get().saveSid(request, response, token.getSid());

		if (requestedUrl != null) {
			StringBuilder successUrl = new StringBuilder(requestedUrl);
			if (requestedUrl.indexOf('?') != -1)
				successUrl.append("&");
			else
				successUrl.append("?");

			Session session = SessionManager.get().get(token.getSid());
			if (session != null) {
				successUrl.append("tenant=");
				successUrl.append(session.getTenantName());
			}

			log.info("Authentication of {} was successful, redirecting to {}", authentication.getName(), successUrl);
			response.setHeader(PARAM_SUCCESSURL, successUrl.toString());
			response.sendRedirect(successUrl.toString());
		}
	}
}