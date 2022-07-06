package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.RunLevel;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for aspects data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class AspectsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(AspectsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

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