package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.metadata.AttributeOption;
import com.logicaldoc.core.metadata.AttributeOptionDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet retrieves the options for extended attributes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class AttributeOptionsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		try {
			long setId = Long.parseLong(request.getParameter("setId"));
			String attribute = request.getParameter("attribute");
			boolean withempty = "true".equals(request.getParameter("withempty"));
			String category = request.getParameter("category");

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			AttributeOptionDAO dao = (AttributeOptionDAO) Context.get().getBean(AttributeOptionDAO.class);
			List<AttributeOption> options = dao.findByAttributeAndCategory(setId, attribute, category);

			if (withempty) {
				if (category == null)
					category = "";

				writer.print("<option>");
				writer.print("<id>-" + category.hashCode() + "</id>");
				writer.print("<attribute></attribute>");
				writer.print("<value></value>");
				writer.print("<position></position>");
				writer.print("<category><![CDATA[" + (StringUtils.isEmpty(category) ? "" : category) + "]]></category>");
				writer.print("</option>");
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
		} catch (NumberFormatException | IOException e) {
			ServletUtil.sendError(response, e.getMessage());
		}
	}
}