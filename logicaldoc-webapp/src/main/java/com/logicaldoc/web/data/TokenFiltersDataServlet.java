package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.analyzer.FilteredAnalyzer;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

/**
 * This servlet is responsible for token filters data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class TokenFiltersDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		String filter = request.getParameter("filter");

		ContextProperties config = Context.get().getProperties();

		PrintWriter writer = response.getWriter();
		writer.print("<list>");

		if (StringUtils.isNotEmpty(filter)) {
			// We have to iterate over the configs of a specific filter
			String prefix = "index.tokenfilter." + filter + ".";
			Map<String, String> settings = config.getProperties(prefix);
			for (Map.Entry<String, String> entry : settings.entrySet()) {
				if (entry.getKey().equals("position"))
					continue;
				writer.print("<filter>");
				writer.print("<name><![CDATA[" + entry.getKey() + "]]></name>");
				writer.print("<value><![CDATA[" + entry.getValue() + "]]></value>");
				writer.print("</filter>");
			}
		} else {
			// We have to iterate over the filters
			List<String> filters = FilteredAnalyzer.getTokenFilterNames(false);
			for (String filterName : filters) {
				writer.print("<filter>");
				writer.print("<name><![CDATA[" + filterName + "]]></name>");
				writer.print("<eenabled>" + "enabled".equals(config.getProperty("index.tokenfilter." + filterName))
						+ "</eenabled>");
				writer.print("</filter>");
			}
		}

		writer.print("</list>");
	}
}