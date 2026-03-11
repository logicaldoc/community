package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		AttributeSetDAO dao = AttributeSetDAO.get();
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
			writer.print(String.format("<id>%d</id>", set.getId()));
			writer.print(String.format("<name><![CDATA[%s]]></name>", set.getName()));
			writer.print(String.format("<label><![CDATA[%s]]></label>",
					StringUtils.defaultIfEmpty(set.getLabel(), set.getName())));
			writer.print(String.format("<description><![CDATA[%s]]></description>",
					StringUtils.defaultString(set.getDescription())));
			writer.print(String.format("<readonly>%b</readonly>", set.isReadonly()));
			writer.print(String.format("<type>%d</type>", set.getType()));
			writer.print("</attributeset>");
		}

		writer.write("</list>");
	}
}