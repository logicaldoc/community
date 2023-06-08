package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for retrieving all the environmen variable
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class EnvironmentDataServlet extends AbstractDataServlet {

	private static final String ENTRY_CLOSE = "</entry>";

	private static final String ENTRY_SCOPE_DATABASE_SCOPE = "<entry><scope>database</scope>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		Map<String, String> env = System.getenv();
		for (Map.Entry<String, String> entry : env.entrySet()) {
			writer.print("<entry><scope>system</scope>");
			writer.print("<name><![CDATA[" + entry.getKey() + "]]></name>");
			writer.print("<value><![CDATA[" + entry.getValue() + "]]></value>");
			writer.print(ENTRY_CLOSE);
		}

		Properties props = System.getProperties();
		for (Map.Entry<Object, Object> entry : props.entrySet()) {
			writer.print("<entry><scope>java</scope>");
			writer.print("<name><![CDATA[" + entry.getKey() + "]]></name>");
			writer.print("<value><![CDATA[" + entry.getValue() + "]]></value>");
			writer.print(ENTRY_CLOSE);
		}

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Map<String, String> meta = dao.getDatabaseMetadata();
		for (Map.Entry<String, String> entry : meta.entrySet()) {
			writer.print(ENTRY_SCOPE_DATABASE_SCOPE);
			writer.print("<name>" + entry.getKey() + "</name>");
			writer.print("<value><![CDATA[" + entry.getValue() + "]]></value>");
			writer.print(ENTRY_CLOSE);
		}

		writer.write("</list>");
	}
}