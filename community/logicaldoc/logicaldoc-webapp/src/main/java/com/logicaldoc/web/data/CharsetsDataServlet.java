package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.LocaleUtil;

/**
 * This servlet is responsible for listings the available charsets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class CharsetsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(CharsetsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			String locale = request.getParameter("locale");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.print("<list>");
			writer.print("<charset>");
			writer.print("<code>auto</code>");
			writer.print("<name>auto</name>");
			writer.print("</charset>");
			Map<String, Charset> charsets = Charset.availableCharsets();
			for (String name : charsets.keySet()) {
				writer.print("<charset>");
				writer.print("<code><![CDATA[" + name + "]]></code>");
				writer.print("<name><![CDATA[" + charsets.get(name).displayName(LocaleUtil.toLocale(locale))
						+ "]]></name>");
				writer.print("</charset>");
			}

			writer.print("</list>");
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
