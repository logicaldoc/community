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

import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet retrieves the options for extended attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOptionsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(AttributeOptionsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			long setId = Long.parseLong(request.getParameter("setId"));
			String attribute = request.getParameter("attribute");
			boolean withempty = "true".equals(request.getParameter("withempty"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
			List<AttributeOption> options = dao.findBySetIdAndAttribute(setId, attribute);

			if (withempty) {
				writer.print("<option>");
				writer.print("<id>-1</id>");
				writer.print("<attribute></attribute>");
				writer.print("<value></value>");
				writer.print("<position></position>");
				writer.print("</option>");
			}

			for (AttributeOption option : options) {
				writer.print("<option>");
				writer.print("<id>" + option.getId() + "</id>");
				writer.print("<attribute><![CDATA[" + option.getAttribute() + "]]></attribute>");
				writer.print("<value><![CDATA[" + option.getValue() + "]]></value>");
				writer.print("<position><![CDATA[" + option.getPosition() + "]]></position>");
				writer.print("</option>");
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
