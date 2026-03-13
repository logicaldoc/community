package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.GroupType;
import com.logicaldoc.i18n.I18N;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		String query = """
                       select A.id, A.name, A.description, A.source "
                         from com.logicaldoc.core.security.user.Group A 
                        where A.deleted = 0 
                          and A.type = %d
                          and A.tenantId = %d
                       """.formatted(GroupType.DEFAULT.ordinal(), session.getTenantId());

		List<?> records = GroupDAO.get().findByQuery(query, (Map<String, Object>) null, null);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			writer.print("<group>");
			writer.print(String.format("<id>%d</id>", (Long) cols[0]));
			writer.print(String.format("<name><![CDATA[%s]]></name>", cols[1]));
			writer.print(String.format("<description><![CDATA[%s]]></description>",
					StringUtils.defaultString((String) cols[2])));
			writer.print(String.format("<label><![CDATA[%s: %s]]></label>", I18N.message("group", locale), cols[1]));
			writer.print(String.format("<source><![CDATA[%s]]></source>", StringUtils.defaultString((String) cols[3])));
			writer.print("</group>");
		}

		writer.write("</list>");
	}
}
