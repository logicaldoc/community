package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for listings the available charsets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class CharsetsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		PrintWriter writer = response.getWriter();
		writer.print("<list>");
		writer.print("<charset>");
		writer.print("<code>auto</code>");
		writer.print("<name>auto</name>");
		writer.print("</charset>");
		Map<String, Charset> charsets = Charset.availableCharsets();
		for (Map.Entry<String, Charset> entry : charsets.entrySet()) {
			writer.print("<charset>");
			writer.print("<code><![CDATA[" + entry.getKey() + "]]></code>");
			writer.print("<name><![CDATA[" + entry.getValue().displayName(locale) + "]]></name>");
			writer.print("</charset>");
		}

		writer.print("</list>");
	}
}