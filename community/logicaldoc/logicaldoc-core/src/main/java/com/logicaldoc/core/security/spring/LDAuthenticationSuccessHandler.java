package com.logicaldoc.core.security.spring;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.util.Context;

/**
 * This handler gets the j_successurl request parameter and use it's value to
 * redirect the user after a successful login.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class LDAuthenticationSuccessHandler implements AuthenticationSuccessHandler {
	private static Logger log = LoggerFactory.getLogger(LDAuthenticationSuccessHandler.class);

	private static final String PARAM_SUCCESSURL = "j_successurl";

	@Override
	public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
			Authentication authentication) throws IOException, ServletException {
		String param = request.getParameter(PARAM_SUCCESSURL);
		LDAuthenticationToken token = (LDAuthenticationToken) authentication;

		Cookie sidCookie = new Cookie(LDAuthenticationToken.COOKIE_SID, token.getSid());
		sidCookie.setHttpOnly(true);
		response.addCookie(sidCookie);

		if (param != null) {
			StringBuffer successUrl = new StringBuffer(param);
			if (param.indexOf('?') != -1)
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

			// Remove the failed login counters
			if (Context.get().getProperties().getBoolean("throttle.enabled", false)) {
				SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
				sDao.delete("loginfail-username-" + session.getUsername(), 0L, Tenant.SYSTEM_ID);
				sDao.delete("loginfail-ip-" + request.getRemoteAddr(), 0L, Tenant.SYSTEM_ID);
			}
		}
	}
}
