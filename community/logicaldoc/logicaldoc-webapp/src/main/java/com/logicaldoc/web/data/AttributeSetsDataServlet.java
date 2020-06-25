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

import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for attribute sets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(AttributeSetsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			Integer type = null;
			if (request.getParameter("type") != null)
				type = Integer.parseInt(request.getParameter("type"));

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			if ("true".equals(request.getParameter("withempty"))) {
				writer.print("<attributeset>");
				writer.print("<id></id>");
				writer.print("<name> </name>");
				writer.print("<readonly>false</readonly>");
				writer.print("</attributeset>");
			}

			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			List<AttributeSet> sets = null;
			if (type != null)
				sets = dao.findByType(type, session.getTenantId());
			else
				sets = dao.findAll(session.getTenantId());

			/*
			 * Iterate over the collection of templates
			 */
			for (AttributeSet set : sets) {

				writer.print("<attributeset>");
				writer.print("<id>" + set.getId() + "</id>");
				writer.print("<name><![CDATA[" + set.getName() + "]]></name>");
				writer.print("<description><![CDATA[" + set.getDescription() + "]]></description>");
				writer.print("<readonly>" + Boolean.toString(set.getReadonly() == 1) + "</readonly>");
				writer.print("<type>" + set.getType() + "</type>");
				writer.print("</attributeset>");
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