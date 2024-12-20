package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet retrieves the users currently logged in
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.0.1
 */
public class OnlineUsersDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		UserDAO userDao = Context.get(UserDAO.class);
		SessionDAO sessionDao = Context.get(SessionDAO.class);
		Session currentSession = ServletUtil.validateSession(request);
		String tenant = currentSession.getTenantName();

		List<Session> sessions = sessionDao.findByNode(null);
		Set<User> users = new HashSet<>();
		for (Session sess : sessions) {
			if (sess.getStatus() != Session.STATUS_OPEN || !tenant.equals(sess.getTenantName()))
				continue;

			User user = userDao.findByUsername(sess.getUsername());
			if (user != null && !users.contains(user)) {
				users.add(user);
			}
		}
		
		PrintWriter writer = response.getWriter();
		writer.print("<list>");
		for (User user : users) {
			writer.print("<user>");
			writer.print("<id>" + user.getId() + "</id>");
			writer.print("<username><![CDATA[" + user.getUsername() + "]]></username>");
			writer.print("<user><![CDATA[" + user.getFullName() + "]]></user>");
			writer.print("</user>");
		}
		writer.print("</list>");
	}
}