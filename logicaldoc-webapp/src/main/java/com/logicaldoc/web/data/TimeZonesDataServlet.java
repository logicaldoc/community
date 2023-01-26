package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;

/**
 * This servlet prints all the available time zones
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
public class TimeZonesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {
		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		// Avoid resource caching
		response.setHeader("Pragma", "no-cache");
		response.setHeader("Cache-Control", "no-store");
		response.setDateHeader("Expires", 0);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		String[] timezones = TimeZone.getAvailableIDs();
		for (String timezone : timezones) {
			writer.print("<timezone>");
			writer.print("<id>" + timezone + "</id>");
			writer.print("</timezone>");
		}
		writer.write("</list>");
	}
}