package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.SessionDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet retrieves the users currently logged in
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.0.1
 */
public class OnlineUsersDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(OnlineUsersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			// Headers required by Internet Explorer
			response.setHeader("Pragma", "public");
			response.setHeader("Cache-Control", "must-revalidate, post-check=0,pre-check=0");
			response.setHeader("Expires", "0");

			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			SessionDAO sessionDao = (SessionDAO) Context.get().getBean(SessionDAO.class);
			Session currentSession = ServiceUtil.validateSession(request);
			String tenant = currentSession.getTenantName();

			List<Session> sessions = sessionDao.findByNode(null);
			Set<User> users = new HashSet<User>();
			for (Session session : sessions) {
				if (session.getStatus() != Session.STATUS_OPEN)
					continue;
				if (!tenant.equals(session.getTenantName()))
					continue;

				User user = userDao.findByUsername(session.getUsername());
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
				writer.print("<avatar>" + user.getId() + "</avatar>");
				writer.print("</user>");
			}
			writer.print("</list>");
		} catch (InvalidSessionException se) {
			// By the way no need to treat this as error
			log.debug(se.getMessage());
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}