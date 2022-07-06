package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for templte rights data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class TemplateRightsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplateRightsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			ServiceUtil.validateSession(request);

			long workflowId = Long.parseLong(request.getParameter("templateId"));

			String locale = request.getParameter("locale");
			if (StringUtils.isEmpty(locale))
				locale = "en";

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			templateRights(response, workflowId, locale);
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

	/**
	 * Useful method for retrieving the label for the users
	 */
	private Map<Long, String> getUsers(long tenantId) throws PersistenceException {
		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		SqlRowSet set = dao.queryForRowSet(
				"select ld_id, ld_username, ld_firstname, ld_name from ld_user where ld_deleted=0 and ld_tenantid="
						+ tenantId,
				null, null);
		Map<Long, String> users = new HashMap<Long, String>();
		while (set.next())
			users.put(set.getLong(1), set.getString(4) + " " + set.getString(3) + " (" + set.getString(2) + ")");
		return users;
	}

	private void templateRights(HttpServletResponse response, Long workflowId, String locale)
			throws IOException, PersistenceException {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = tDao.findById(workflowId);
		tDao.initialize(template);

		// Prepare a map of users
		Map<Long, String> users = getUsers(template.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the folder group in join with groups
		StringBuffer query = new StringBuffer(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write from ld_templategroup as A, ld_group B where A.ld_templateid = ");
		query.append("" + template.getId());
		query.append(" and B.ld_tenantid = " + template.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_name asc");

		SqlRowSet set = tDao.queryForRowSet(query.toString(), null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next()) {
			long groupId = set.getLong(1);
			String groupName = set.getString(2);
			int groupType = set.getInt(3);
			long userId = 0L;
			if (groupType == Group.TYPE_USER)
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
			writer.print("<write>" + (set.getInt(4) == 1 ? true : false) + "</write>");
			writer.print("<type>" + groupType + "</type>");
			writer.print("</right>");

		}

		writer.write("</list>");
	}
}