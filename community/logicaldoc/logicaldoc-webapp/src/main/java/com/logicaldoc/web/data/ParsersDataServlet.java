package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;


/**
 * This servlet is responsible for parsers data.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class ParsersDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ParsersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);
			ContextProperties config = Context.get().getProperties();

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");
			int i = 0;
			Set<String> keys = ParserFactory.getParsers().keySet();
			List<String> sort = new ArrayList<String>();
			for (String ext : keys) {
				sort.add(ext);
			}
			Collections.sort(sort);

			for (String ext : sort) {
				writer.print("<parser>");
				writer.print("<id>" + i + "</id>");
				writer.print("<icon>" + FilenameUtils.getBaseName(IconSelector.selectIcon(ext.toLowerCase()))
						+ "</icon>");
				writer.print("<extension>" + ext.toLowerCase() + "</extension>");
				writer.print("<name><![CDATA[" + ParserFactory.getParsers().get(ext).getSimpleName() + "]]></name>");

				String aliasProp = config.getProperty("parser.alias." + ext.toLowerCase());
				writer.print("<aliases><![CDATA[" + (aliasProp != null ? aliasProp : "") + "]]></aliases>");
				writer.print("</parser>");
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