package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for groups data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GroupsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(GroupsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			String locale = request.getParameter("locale");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			StringBuffer query = new StringBuffer("select A.id, A.name, A.description, A.source "
					+ "from com.logicaldoc.core.security.Group A where A.deleted = 0 and A.type = "
					+ Group.TYPE_DEFAULT + " and A.tenantId=" + session.getTenantId());

			GroupDAO dao = (GroupDAO) Context.get().getBean(GroupDAO.class);
			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), (Map<String, Object>) null, null);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<group>");
				writer.print("<id>" + cols[0] + "</id>");
				writer.print("<name><![CDATA[" + cols[1] + "]]></name>");
				writer.print("<description><![CDATA[" + (cols[2] == null ? "" : cols[2]) + "]]></description>");
				writer.print("<label><![CDATA[" + I18N.message("group", locale) + ": " + (String) cols[1]
						+ "]]></label>");
				writer.print("<source><![CDATA[" + (cols[3] == null ? "" : cols[3]) + "]]></source>");
				writer.print("</group>");
			}

			writer.write("</list>");
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
