package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.StringUtil;

/**
 * This servlet is responsible for users data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UsersDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		String groupIdOrName = request.getParameter("groupId");
		boolean required = "true".equals(request.getParameter("required"));
		boolean skipdisabled = "true".equals(request.getParameter("skipdisabled"));

		List<User> users = findUsers(session, groupIdOrName);

		printUsers(users, required, skipdisabled, response);
	}

	private void printUsers(List<User> users, boolean required, boolean skipdisabled, HttpServletResponse response)
			throws IOException {
		PrintWriter writer = response.getWriter();
		writer.print("<list>");
		if (!required)
			writer.print("<user><id></id><username></username><name></name></user>");

		/*
		 * Iterate over records composing the response XML document
		 */
		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		for (User user : users) {
			if (user.getType() == User.TYPE_SYSTEM)
				continue;

			if (skipdisabled && user.getEnabled() != 1)
				continue;

			userDao.initialize(user);

			printUser(writer, user);
		}

		writer.print("</list>");
	}

	private void printUser(PrintWriter writer, User user) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));

		writer.print("<user>");
		writer.print("<id>" + user.getId() + "</id>");
		writer.print("<username><![CDATA[" + user.getUsername() + "]]></username>");
		writer.print("<enabledIcon>" + (user.getEnabled() == 1 ? "0" : "2") + "</enabledIcon>");
		writer.print("<eenabled>" + (user.getEnabled() == 1 ? "true" : "false") + "</eenabled>");
		writer.print("<guest>" + user.isReadonly() + "</guest>");
		writer.print("<name><![CDATA[" + (user.getName() == null ? "" : user.getName()) + "]]></name>");
		writer.print(
				"<firstName><![CDATA[" + (user.getFirstName() == null ? "" : user.getFirstName()) + "]]></firstName>");
		writer.print("<label><![CDATA[" + (user.getFullName() == null ? "" : user.getFullName()) + "]]></label>");
		writer.print("<email><![CDATA[" + (user.getEmail() == null ? "" : user.getEmail()) + "]]></email>");
		writer.print("<phone><![CDATA[" + (user.getTelephone() == null ? "" : user.getTelephone()) + "]]></phone>");
		writer.print("<cell><![CDATA[" + (user.getTelephone2() == null ? "" : user.getTelephone2()) + "]]></cell>");
		if (user.getExpire() != null)
			writer.print("<expire>" + df.format(user.getExpire()) + "</expire>");
		if (user.getUserGroup() != null)
			writer.print("<usergroup><![CDATA[" + user.getUserGroup().getId() + "]]></usergroup>");

		String[] groups = user.getGroupNames();
		groups = Arrays.stream(user.getGroupNames()).filter(g -> !g.startsWith("_user_")).toArray(String[]::new);
		writer.print("<groups><![CDATA[" + StringUtil.arrayToString(groups, ", ") + "]]></groups>");
		writer.print("<avatar>" + user.getId() + "</avatar>");

		if (user.getTimeZone() != null)
			writer.print("<timeZone><![CDATA[" + user.getTimeZone() + "]]></timeZone>");
		writer.print("</user>");
	}

	private List<User> findUsers(Session session, String groupIdOrName) throws PersistenceException {
		List<User> users = new ArrayList<User>();

		UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
		GroupDAO groupDao = (GroupDAO) Context.get().getBean(GroupDAO.class);

		if (groupIdOrName != null && !groupIdOrName.trim().isEmpty()) {
			Group group = null;
			try {
				group = groupDao.findById(Long.parseLong(groupIdOrName));
			} catch (Throwable t) {
				// Nothing to do
			}
			if (group == null)
				group = groupDao.findByName(groupIdOrName, session.getTenantId());
			groupDao.initialize(group);

			users.addAll(group.getUsers());
		} else {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("tenantId", session.getTenantId());

			users = userDao.findByWhere("_entity.tenantId = :tenantId", params, null, null);
		}
		return users;
	}
}