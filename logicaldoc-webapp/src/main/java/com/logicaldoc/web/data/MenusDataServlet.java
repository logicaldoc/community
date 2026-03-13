package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for menus data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MenusDataServlet extends AbstractDataServlet {

	private static final String PARENT = "parent";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		MenuDAO dao = MenuDAO.get();
		long parent = Menu.ROOT;

		if (!"/".equals(request.getParameter(PARENT)) && StringUtils.isNotEmpty(request.getParameter(PARENT)))
			parent = Long.parseLong(request.getParameter(PARENT));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Get the visible children
		 */
		List<Menu> menus = dao.findByUserId(session.getUserId(), parent, false);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Menu menu : menus) {
			writer.print("<menu>");
			writer.print(String.format("<id>%d</id>", menu.getId()));
			writer.print(String.format("<name><![CDATA[%s]]></name>", menu.getName()));
			writer.print(String.format("<position><![CDATA[%s]]></position>", menu.getPosition()));
			writer.print(String.format("<parent>%s</parent>", menu.getParentId()));
			writer.print(String.format("<eenabled>%b</eenabled>", menu.isEnabled()));
			writer.print("</menu>");
		}

		writer.write("</list>");
	}
}