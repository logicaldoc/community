package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;

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
		BookmarkDAO dao = Context.get(BookmarkDAO.class);

		/*
		 * Search for folders first.
		 */
		StringBuilder query = new StringBuilder("select A.id, A.name, A.title, A.type, A.query, A.content"
				+ " from Dashlet A where A.deleted = 0 and A.tenantId = " + session.getTenantId()
				+ " order by A.id asc");
		records.addAll(dao.findByQuery(query.toString(), (Map<String, Object>) null, null));

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			writer.print("<dashlet>");
			writer.print("<id>" + cols[0] + "</id>");
			writer.print("<name><![CDATA[" + (cols[1] == null ? "" : cols[1]) + "]]></name>");
			writer.print("<title><![CDATA[" + (cols[2] == null ? "" : I18N.message(cols[2].toString(), locale))
					+ "]]></title>");
			writer.print("<type>" + (cols[3] == null ? "" : cols[3]) + "</type>");
			writer.print("<query><![CDATA[" + (cols[4] == null ? "" : cols[4]) + "]]></query>");
			writer.print("<content><![CDATA[" + (cols[5] == null ? "" : cols[5]) + "]]></content>");
			writer.print("</dashlet>");
		}

		writer.write("</list>");
	}
}