package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.runtime.RunLevel;
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
			writer.print(String.format("<id>%s</id>",aspect));
			writer.print(String.format("<default>%b</default>", RunLevel.DEFAULT.aspectEnabled(aspect)));
			writer.print(String.format("<bulkload>%b</bulkload>", RunLevel.BULKLOAD.aspectEnabled(aspect)));
			writer.print(String.format("<devel>%b</devel>", RunLevel.DEVEL.aspectEnabled(aspect)));
			writer.print(String.format("<slave>%b</slave>", RunLevel.SLAVE.aspectEnabled(aspect)));
			writer.print(String.format("<demo>%b</demo>", RunLevel.DEMO.aspectEnabled(aspect)));
			writer.print("</aspect>");
		}
		writer.write("</list>");
	}
}