package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.GroupType;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.spring.Context;

/**
 * This servlet is responsible for groups data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GroupsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder("select A.id, A.name, A.description, A.source "
				+ "from com.logicaldoc.core.security.user.Group A where A.deleted = 0 and A.type = "
				+ GroupType.DEFAULT.ordinal() + " and A.tenantId=" + session.getTenantId());

		GroupDAO dao = Context.get(GroupDAO.class);
		List<?> records = dao.findByQuery(query.toString(), (Map<String, Object>) null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			writer.print("<group>");
			writer.print("<id>" + cols[0] + "</id>");
			writer.print("<name><![CDATA[" + cols[1] + "]]></name>");
			writer.print("<description><![CDATA[" + (cols[2] == null ? "" : cols[2]) + "]]></description>");
			writer.print("<label><![CDATA[" + I18N.message("group", locale) + ": " + (String) cols[1] + "]]></label>");
			writer.print("<source><![CDATA[" + (cols[3] == null ? "" : cols[3]) + "]]></source>");
			writer.print("</group>");
		}

		writer.write("</list>");
	}
}
