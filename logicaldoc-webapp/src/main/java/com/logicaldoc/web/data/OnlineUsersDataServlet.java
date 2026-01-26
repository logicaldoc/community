package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		Session currentSession = ServletUtil.validateSession(request);
		String tenant = currentSession.getTenantName();

		List<Session> sessions = SessionDAO.get().findByNode(null);
		Set<User> users = new HashSet<>();
		UserDAO userDao = UserDAO.get();
		for (Session sess : sessions) {
			if (!sess.isOpen() || !tenant.equals(sess.getTenantName()))
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
			writer.print(String.format("<id>%d</id>", user.getId()));
			writer.print(String.format("<username><![CDATA[%s]]></username>",user.getUsername()));
			writer.print(String.format("<user><![CDATA[%s]]></user>",user.getFullName()));
			writer.print(String.format("</user>"));
		}
		writer.print("</list>");
	}
}