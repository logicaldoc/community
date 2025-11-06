package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for retrieving all the extension aliases.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ExtensionAliasesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max, Locale locale)
			throws PersistenceException, IOException {

		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		manager.getConverters();

		ContextProperties config = Context.get().getProperties();

		Map<String, String> aliasMap = config.getProperties("converter.alias");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Get all the possible associations of a specific converter
		for (String targetExt : manager.getAvailableInputFormats()) {
			writer.print("<alias>");
			writer.print("<extension><![CDATA[" + targetExt + "]]></extension>");

			// Get all the keys that correspond to the same target
			// extension
			Set<String> keys = aliasMap.entrySet().stream().filter(entry -> Objects.equals(entry.getValue(), targetExt))
					.map(Map.Entry::getKey).collect(Collectors.toSet());

			// Now extract just the extension and produce a comma separated
			// values string
			String aliasExts = keys.stream().map(key -> key.substring(key.lastIndexOf('.') + 1))
					.collect(Collectors.joining(","));

			writer.print("<aliases><![CDATA[" + aliasExts + "]]></aliases>");
			writer.print("</alias>");
		}

		writer.write("</list>");
	}
}
