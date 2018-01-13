package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for templates data.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class TemplatesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(TemplatesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			String folderId = request.getParameter("folderId");

			Integer type = null;
			if (request.getParameter("type") != null)
				type = new Integer(request.getParameter("type"));

			List<Long> templateIds = new ArrayList<Long>();
			if (StringUtils.isNotEmpty(folderId)) {
				GenericDAO genericDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
				// Get all the 'wf-trigger' generics on this folder
				List<Generic> triggerGenerics = genericDao.findByTypeAndSubtype("wf-trigger", folderId + "-%", null,
						session.getTenantId());
				// Retrieve all the ids of the templates associated to a
				// workflow
				// already associated on the given folder
				for (Generic generic : triggerGenerics) {
					String templateId = generic.getSubtype().substring(generic.getSubtype().indexOf("-") + 1);
					if (StringUtils.isNotEmpty(templateId))
						templateIds.add(Long.parseLong(templateId));
				}
			}

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

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

			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			List<Template> templates = null;
			if (type != null)
				templates = dao.findByType(type, session.getTenantId());
			else
				templates = dao.findAll(session.getTenantId());

			/*
			 * Iterate over the collection of templates
			 */
			for (Template template : templates) {
				if (templateIds.contains(template.getId()))
					continue;

				writer.print("<template>");
				writer.print("<id>" + template.getId() + "</id>");
				writer.print("<name><![CDATA[" + template.getName() + "]]></name>");
				writer.print("<description><![CDATA[" + template.getDescription() + "]]></description>");
				writer.print("<readonly>" + Boolean.toString(template.getReadonly() == 1) + "</readonly>");
				writer.print("<type>" + template.getType() + "</type>");
				writer.print("</template>");
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