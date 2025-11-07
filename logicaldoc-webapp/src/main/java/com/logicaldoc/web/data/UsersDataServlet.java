package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.GroupType;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserType;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
			throws IOException, PersistenceException {
		PrintWriter writer = response.getWriter();
		writer.print("<list>");
		if (!required)
			writer.print("<user><id></id><username></username><name></name></user>");

		/*
		 * Iterate over records composing the response XML document
		 */
		UserDAO userDao = UserDAO.get();
		for (User user : users) {
			if (user.getType() == UserType.SYSTEM || (skipdisabled && user.getEnabled() != 1))
				continue;

			userDao.initialize(user);

			printUser(writer, user);
		}

		writer.print("</list>");
	}

	private void printUser(PrintWriter writer, User user) throws PersistenceException {
		DateFormat df = getDateFormat();

		writer.print("<user>");
		writer.print("<id>" + user.getId() + "</id>");
		writer.print("<username><![CDATA[" + user.getUsername() + "]]></username>");
		writer.print("<eenabled>" + Boolean.toString(user.getEnabled() == 1) + "</eenabled>");
		writer.print("<guest>" + user.isReadonly() + "</guest>");
		writer.print("<name><![CDATA[" + StringUtils.defaultString(user.getName()) + "]]></name>");
		writer.print("<firstName><![CDATA[" + StringUtils.defaultString(user.getFirstName()) + "]]></firstName>");
		writer.print("<label><![CDATA[" + StringUtils.defaultString(user.getFullName()) + "]]></label>");
		writer.print("<email><![CDATA[" + StringUtils.defaultString(user.getEmail()) + "]]></email>");
		writer.print("<phone><![CDATA[" + StringUtils.defaultString(user.getTelephone()) + "]]></phone>");
		writer.print("<cell><![CDATA[" + StringUtils.defaultString(user.getTelephone2()) + "]]></cell>");
		writer.print("<city><![CDATA[" + StringUtils.defaultString(user.getCity()) + "]]></city>");
		writer.print("<department><![CDATA[" + StringUtils.defaultString(user.getDepartment()) + "]]></department>");
		writer.print("<building><![CDATA[" + StringUtils.defaultString(user.getBuilding()) + "]]></building>");
		writer.print("<organizationalUnit><![CDATA[" + StringUtils.defaultString(user.getOrganizationalUnit())
				+ "]]></organizationalUnit>");
		writer.print("<company><![CDATA[" + StringUtils.defaultString(user.getCompany()) + "]]></company>");

		writer.print("<source>" + user.getSource().name() + "</source>");
		if (user.getExpire() != null)
			writer.print("<expire>" + df.format(user.getExpire()) + "</expire>");
		if (user.getLastLogin() != null)
			writer.print("<lastLogin>" + df.format(user.getLastLogin()) + "</lastLogin>");
		if (user.getCreation() != null)
			writer.print("<creation>" + df.format(user.getCreation()) + "</creation>");
		if (user.getUserGroup() != null)
			writer.print("<usergroup><![CDATA[" + user.getUserGroup().getId() + "]]></usergroup>");

		writer.print("<groups><![CDATA[" + user.getGroups().stream().filter(g -> g.getType() == GroupType.DEFAULT)
				.map(Group::getName).collect(Collectors.joining(", ")) + "]]></groups>");
		writer.print("<avatar>" + user.getId() + "</avatar>");
		writer.print("<sfa>" +  StringUtils.defaultString(user.getSecondFactor()) + "</sfa>");

		if (user.getTimeZone() != null)
			writer.print("<timeZone><![CDATA[" + user.getTimeZone() + "]]></timeZone>");
		writer.print("</user>");
	}

	private List<User> findUsers(Session session, String groupIdOrName) throws PersistenceException {
		List<User> users = new ArrayList<>();

		UserDAO userDao = UserDAO.get();
		GroupDAO groupDao = GroupDAO.get();

		if (groupIdOrName != null && !groupIdOrName.trim().isEmpty()) {
			Group group = null;
			try {
				group = groupDao.findById(Long.parseLong(groupIdOrName));
			} catch (Exception t) {
				// Nothing to do
			}
			if (group == null)
				group = groupDao.findByName(groupIdOrName, session.getTenantId());
			groupDao.initialize(group);

			users.addAll(group.getUsers());
		} else {
			Map<String, Object> params = new HashMap<>();
			params.put("tenantId", session.getTenantId());

			users = userDao.findByWhere("_entity.tenantId = :tenantId", params, null, null);
		}
		return users;
	}
}