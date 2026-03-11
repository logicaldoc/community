package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet retrieves the options for extended attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOptionsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected boolean isSessionRequired() {
		return false;
	}

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		long setId = Long.parseLong(request.getParameter("setId"));
		String attribute = request.getParameter("attribute");
		boolean withempty = "true".equals(request.getParameter("withempty"));
		String category = request.getParameter("category");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		AttributeOptionDAO dao = AttributeOptionDAO.get();
		List<AttributeOption> options = dao.findByAttributeAndCategory(setId, attribute, category);

		if (withempty) {
			if (category == null)
				category = "";

			writer.print("<option>");
			writer.print(String.format("<id>-%d</id>", category.hashCode()));
			writer.print("<attribute></attribute>");
			writer.print("<value></value>");
			writer.print("<position></position>");
			writer.print(String.format("<category><![CDATA[%s]]></category>", StringUtils.defaultString(category)));
			writer.print("</option>");
		}

		for (AttributeOption option : options) {
			writer.print("<option>");
			writer.print(String.format("<id>%d</id>", option.getId()));
			writer.print(String.format("<attribute><![CDATA[%s]]></attribute>", option.getAttribute()));
			writer.print(String.format("<value><![CDATA[%s]]></value>", option.getValue()));
			if (StringUtils.isNotEmpty(option.getCategory()))
				writer.print(String.format("<category><![CDATA[%s]]></category>", option.getCategory()));
			writer.print(String.format("<position><![CDATA[%d]]></position>", option.getPosition()));
			writer.print("</option>");
		}

		writer.write("</list>");
	}
}