package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for templates data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TemplatesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplatesDataServlet.class);

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

			Session session = ServiceUtil.validateSession(request);
			Long templateId = StringUtils.isNotEmpty(request.getParameter("templateId"))
					? Long.parseLong(request.getParameter("templateId"))
					: null;
			Integer type = request.getParameter("type") != null ? Integer.parseInt(request.getParameter("type")) : null;

			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = templateId != null ? templateDao.findById(templateId) : null;

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			if ("true".equals(request.getParameter("withempty"))) {
				writer.print("<template>");
				writer.print("<id></id>");
				writer.print("<name> </name>");
				writer.print("<documents>0</documents>");
				writer.print("<readonly>false</readonly>");
				writer.print("</template>");
			}

			List<Template> templates = null;
			if (type != null)
				templates = templateDao.findByType(type, session.getTenantId());
			else
				templates = templateDao.findAll(session.getTenantId());

			/*
			 * Iterate over the collection of templates
			 */
			for (Template templ : templates) {
				if (templateDao.isReadEnable(templ.getId(), session.getUserId())
						|| (template != null && template.equals(templ))) {

					writer.print("<template>");
					writer.print("<id>" + templ.getId() + "</id>");
					writer.print("<name><![CDATA[" + templ.getName() + "]]></name>");
					writer.print("<description><![CDATA[" + templ.getDescription() + "]]></description>");
					writer.print("<readonly>" + Boolean.toString(templ.getReadonly() == 1) + "</readonly>");
					writer.print("<type>" + templ.getType() + "</type>");
					writer.print("</template>");
				}
			}

			writer.write("</list>");
		} catch (

		Throwable e) {
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