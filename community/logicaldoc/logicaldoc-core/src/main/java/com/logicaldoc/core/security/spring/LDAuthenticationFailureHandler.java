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

import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.security.dao.UserHistoryDAO;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.util.Context;

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
		failureUrl.append(exception.getMessage());

		Cookie failureCookie = new Cookie(COOKIE_LDOC_FAILURE, exception.getMessage());
		response.addCookie(failureCookie);

		String username = request.getParameter("j_username");
		log.warn("Authentication of '" + username + "' was unsuccesful");

		// Record the failed login attempt
		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		User user = uDao.findByUsername(username);
		if (user == null) {
			user = new User();
			user.setUsername(username);
			user.setName(username);
		}
		UserHistoryDAO dao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		dao.createUserHistory(user, UserHistory.EVENT_USER_LOGIN_FAILED, request.getRemoteAddr(),
				request.getRemoteAddr(), null);

		// Update the failed login counters
		if ("true".equals(Context.get().getProperties().getProperty("throttle.enabled"))) {
			SequenceDAO sDao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			sDao.next("loginfail-username-" + username, 0L, Tenant.SYSTEM_ID);
			sDao.next("loginfail-ip-" + request.getRemoteAddr(), 0L, Tenant.SYSTEM_ID);
		}

		log.info("Redirecting to " + failureUrl.toString());
		response.sendRedirect(failureUrl.toString());
	}
}