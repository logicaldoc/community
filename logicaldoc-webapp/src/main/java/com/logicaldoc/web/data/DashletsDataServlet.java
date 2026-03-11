package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for dashlets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		/*
		 * Iterate over the collection of bookmarks
		 */
		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		List<Object> records = new ArrayList<>();

		/*
		 * Search for folders first.
		 */
		String query = "select A.id, A.name, A.title, A.type, A.query, A.content from Dashlet A where A.deleted = 0 and A.tenantId = %d order by A.id asc"
				.formatted(session.getTenantId());
		records.addAll(BookmarkDAO.get().findByQuery(query.toString(), (Map<String, Object>) null, null));

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			writer.print("<dashlet>");
			writer.print(String.format("<id>%d</id>", cols[0]));
			writer.print(String.format("<name><![CDATA[%s]]></name>", cols[1] == null ? "" : cols[1]));
			writer.print(String.format("<title><![CDATA[%s]]></title>",
					cols[2] == null ? "" : I18N.message(cols[2].toString(), locale)));
			writer.print(String.format("<type>%s</type>", cols[3] == null ? "" : cols[3]));
			writer.print(String.format("<query><![CDATA[%s]]></query>", cols[4] == null ? "" : cols[4]));
			writer.print(String.format("<content><![CDATA[%s]]></content>", cols[5] == null ? "" : cols[5]));
			writer.print("</dashlet>");
		}

		writer.write("</list>");
	}
}