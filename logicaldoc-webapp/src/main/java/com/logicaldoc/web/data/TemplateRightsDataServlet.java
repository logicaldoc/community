package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for templte rights data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class TemplateRightsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		long workflowId = Long.parseLong(request.getParameter("templateId"));

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		// Avoid resource caching
		response.setHeader("Pragma", "no-cache");
		response.setHeader("Cache-Control", "no-store");
		response.setDateHeader("Expires", 0);

		templateRights(response, workflowId);
	}

	/**
	 * Useful method for retrieving the label for the users
	 */
	private Map<Long, String> getUsers(long tenantId) throws PersistenceException {
		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		SqlRowSet set = dao.queryForRowSet(
				"select ld_id, ld_username, ld_firstname, ld_name from ld_user where ld_deleted=0 and ld_tenantid="
						+ tenantId,
				null);
		Map<Long, String> users = new HashMap<>();
		while (set.next())
			users.put(set.getLong(1), set.getString(4) + " " + set.getString(3) + " (" + set.getString(2) + ")");
		return users;
	}

	private void templateRights(HttpServletResponse response, Long workflowId)
			throws IOException, PersistenceException {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = tDao.findById(workflowId);
		tDao.initialize(template);

		// Prepare a map of users
		Map<Long, String> users = getUsers(template.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the folder group in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write from ld_templategroup as A, ld_group B where A.ld_templateid = ");
		query.append("" + template.getId());
		query.append(" and B.ld_tenantid = " + template.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_name asc");

		SqlRowSet set = tDao.queryForRowSet(query.toString(), null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next()) {
			long groupId = set.getLong(1);
			String groupName = set.getString(2);
			int groupType = set.getInt(3);
			long userId = 0L;
			if (groupType == Group.TYPE_USER && groupName != null)
				userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

			writer.print("<right>");
			writer.print("<entityId>" + groupId + "</entityId>");

			if (groupType == Group.TYPE_DEFAULT) {
				writer.print("<entity><![CDATA[" + groupName + "]]></entity>");
				writer.print("<avatar>group</avatar>");
			} else {
				writer.print("<entity><![CDATA[" + users.get(userId) + "]]></entity>");
				writer.print("<avatar>" + userId + "</avatar>");
			}
			writer.print("<read>true</read>");
			writer.print("<write>" + (set.getInt(4) == 1) + "</write>");
			writer.print("<type>" + groupType + "</type>");
			writer.print("</right>");

		}

		writer.write("</list>");
	}
}