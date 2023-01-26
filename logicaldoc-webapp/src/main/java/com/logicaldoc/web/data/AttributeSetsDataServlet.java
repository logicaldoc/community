package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for attribute sets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class AttributeSetsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Integer type = null;
		if (request.getParameter("type") != null)
			type = Integer.parseInt(request.getParameter("type"));

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

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
	}
}