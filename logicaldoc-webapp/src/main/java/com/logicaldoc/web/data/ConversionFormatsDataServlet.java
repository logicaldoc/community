package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for conversion formats data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class ConversionFormatsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ConversionFormatsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			String fileName = request.getParameter("fileName");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
					FormatConverterManager.class);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			/*
			 * Iterate over records composing the response XML document
			 */
			for (String format : manager.getEnabledOutputFormats(fileName)) {
				writer.print("<format>");
				writer.print("<extension><![CDATA[" + format + "]]></extension>");
				writer.print("</format>");
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
