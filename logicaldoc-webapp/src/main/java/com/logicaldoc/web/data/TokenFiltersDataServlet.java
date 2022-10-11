package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.searchengine.analyzer.FilteredAnalyzer;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This servlet is responsible for token filters data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class TokenFiltersDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale) throws PersistenceException, IOException {

		String filter = request.getParameter("filter");

		ContextProperties config = Context.get().getProperties();

		PrintWriter writer = response.getWriter();
		writer.print("<list>");

		if (StringUtils.isNotEmpty(filter)) {
			// We have to iterate over the configs of a specific filter
			String prefix = "index.tokenfilter." + filter + ".";
			Map<String, String> settings = config.getProperties(prefix);
			for (String setting : settings.keySet()) {
				if (setting.equals("position"))
					continue;
				writer.print("<filter>");
				writer.print("<name><![CDATA[" + setting + "]]></name>");
				writer.print("<value><![CDATA[" + settings.get(setting) + "]]></value>");
				writer.print("</filter>");
			}
		} else {
			// We have to iterate over the filters
			List<String> filters = FilteredAnalyzer.getTokenFilterNames(false);
			for (String filterName : filters) {
				writer.print("<filter>");
				writer.print("<name><![CDATA[" + filterName + "]]></name>");
				if ("enabled".equals(config.getProperty("index.tokenfilter." + filterName)))
					writer.print("<eenabled>0</eenabled>");
				else
					writer.print("<eenabled>2</eenabled>");
				writer.print("</filter>");
			}
		}

		writer.print("</list>");
	}
}
