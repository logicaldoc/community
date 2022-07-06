package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for retrieving all the extension aliases.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ExtensionAliasesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ExtensionAliasesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
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
				Set<String> keys = aliasMap.entrySet().stream()
						.filter(entry -> Objects.equals(entry.getValue(), targetExt)).map(Map.Entry::getKey)
						.collect(Collectors.toSet());

				// Now extract just the extension and produce a comma separated
				// values string
				String aliasExts = keys.stream().map(key -> key.substring(key.lastIndexOf('.') + 1))
						.collect(Collectors.joining(","));

				writer.print("<aliases><![CDATA[" + aliasExts + "]]></aliases>");
				writer.print("</alias>");
			}

			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}
