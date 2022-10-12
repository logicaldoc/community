package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet retrieves the options for extended attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOptionsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale)
			throws PersistenceException, IOException {

		long setId = Long.parseLong(request.getParameter("setId"));
		String attribute = request.getParameter("attribute");
		boolean withempty = "true".equals(request.getParameter("withempty"));
		String category = request.getParameter("category");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
		List<AttributeOption> options = dao.findByAttributeAndCategory(setId, attribute, category);

		if (withempty) {
			List<String> categories = options.stream().map(o -> o.getCategory()).distinct()
					.collect(Collectors.toList());
			if (!categories.contains(""))
				categories.add("");

			for (String cat : categories) {
				if (cat == null)
					continue;
				writer.print("<option>");
				writer.print("<id>-" + cat.hashCode() + "</id>");
				writer.print("<attribute></attribute>");
				writer.print("<value></value>");
				writer.print("<position></position>");
				writer.print("<category><![CDATA[" + (StringUtils.isEmpty(cat) ? "" : cat) + "]]></category>");
				writer.print("</option>");
			}
		}

		for (AttributeOption option : options) {
			writer.print("<option>");
			writer.print("<id>" + option.getId() + "</id>");
			writer.print("<attribute><![CDATA[" + option.getAttribute() + "]]></attribute>");
			writer.print("<value><![CDATA[" + option.getValue() + "]]></value>");
			if (StringUtils.isNotEmpty(option.getCategory()))
				writer.print("<category><![CDATA[" + option.getCategory() + "]]></category>");
			writer.print("<position><![CDATA[" + option.getPosition() + "]]></position>");
			writer.print("</option>");
		}

		writer.write("</list>");
	}
}