package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for aspects data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class AspectsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max, Locale locale)
			throws PersistenceException, IOException {

		List<String> aspects = RunLevel.getAspects();
		PrintWriter writer = response.getWriter();
		writer.write("<list>");
		for (String aspect : aspects) {
			writer.print("<aspect>");
			writer.print("<id>" + aspect + "</id>");
			writer.print("<default>" + RunLevel.DEFAULT.aspectEnabled(aspect) + "</default>");
			writer.print("<bulkload>" + RunLevel.BULKLOAD.aspectEnabled(aspect) + "</bulkload>");
			writer.print("<devel>" + RunLevel.DEVEL.aspectEnabled(aspect) + "</devel>");
			writer.print("<devel>" + RunLevel.SLAVE.aspectEnabled(aspect) + "</devel>");
			writer.print("<demo>" + RunLevel.DEMO.aspectEnabled(aspect) + "</demo>");
			writer.print("</aspect>");
		}
		writer.write("</list>");
	}
}