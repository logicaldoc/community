package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for users data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UsersDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(UsersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			String groupIdOrName = request.getParameter("groupId");
			boolean required = "true".equals(request.getParameter("required"));

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.print("<list>");

			if (!required)
				writer.print("<user><id></id><username></username><name></name></user>");

			List<User> users = new ArrayList<User>();

			UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
			GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);

			if (groupIdOrName != null && !groupIdOrName.trim().isEmpty()) {
				Group group = null;
				try {
					group = groupDao.findById(Long.parseLong(groupIdOrName));
				} catch (Throwable t) {
				}
				if (group == null)
					group = groupDao.findByName(groupIdOrName, session.getTenantId());
				groupDao.initialize(group);

				users.addAll(group.getUsers());
			} else {
				users = userDao.findByWhere("_entity.tenantId=?1", new Long[] { session.getTenantId() }, null, null);
			}

			/*
			 * Iterate over records composing the response XML document
			 */
			for (User user : users) {
				if (user.getType() != User.TYPE_DEFAULT)
					continue;

				userDao.initialize(user);

				writer.print("<user>");
				writer.print("<id>" + user.getId() + "</id>");
				writer.print("<username><![CDATA[" + user.getUsername() + "]]></username>");
				if (user.getEnabled() == 1)
					writer.print("<eenabled>0</eenabled>");
				else if (user.getEnabled() == 0)
					writer.print("<eenabled>2</eenabled>");
				writer.print("<name><![CDATA[" + (user.getName() == null ? "" : user.getName()) + "]]></name>");
				writer.print("<firstName><![CDATA[" + (user.getFirstName() == null ? "" : user.getFirstName())
						+ "]]></firstName>");
				writer.print("<label><![CDATA[" + (user.getFullName() == null ? "" : user.getFullName())
						+ "]]></label>");
				writer.print("<email><![CDATA[" + (user.getEmail() == null ? "" : user.getEmail()) + "]]></email>");
				writer.print("<phone><![CDATA[" + (user.getTelephone() == null ? "" : user.getTelephone())
						+ "]]></phone>");
				writer.print("<cell><![CDATA[" + (user.getTelephone2() == null ? "" : user.getTelephone2())
						+ "]]></cell>");
				if (user.getUserGroup() != null)
					writer.print("<usergroup><![CDATA[" + user.getUserGroup().getId() + "]]></usergroup>");

				String[] groups = user.getGroupNames();
				groups = Arrays.stream(user.getGroupNames()).filter(g -> !g.startsWith("_user_"))
						.toArray(String[]::new);

				writer.print("<groups><![CDATA[" + StringUtil.arrayToString(groups, ", ") + "]]></groups>");
				writer.print("</user>");
			}

			writer.print("</list>");

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