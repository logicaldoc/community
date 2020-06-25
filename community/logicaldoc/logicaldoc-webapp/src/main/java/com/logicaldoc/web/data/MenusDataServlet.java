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

import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for menus data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MenusDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(MenusDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			Context context = Context.get();
			MenuDAO dao = (MenuDAO) context.getBean(MenuDAO.class);
			long parent = Menu.ROOT;

			if ("/".equals(request.getParameter("parent")))
				parent = Menu.ROOT;
			else if(StringUtils.isNotEmpty(request.getParameter("parent")))
				parent = Long.parseLong(request.getParameter("parent"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			/*
			 * Get the visible children
			 */
			List<Menu> menus = dao.findByUserId(session.getUserId(), parent);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Menu menu : menus) {
				writer.print("<menu>");
				writer.print("<id>" + menu.getId() + "</id>");
				writer.print("<name><![CDATA[" + menu.getName() + "]]></name>");
				writer.print("<position><![CDATA[" + menu.getPosition() + "]]></position>");
				writer.print("<parent>" + menu.getParentId() + "</parent>");
				writer.print("</menu>");
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