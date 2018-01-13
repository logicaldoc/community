package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.store.StorerManager;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for storages data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class StoragesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(StoragesDataServlet.class);

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

			boolean types = "true".equals(request.getParameter("types"));

			boolean parameters = "true".equals(request.getParameter("parameters"));
			if (parameters) {
				MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
				parameters = session.getTenantId() == Tenant.DEFAULT_ID && mDao.isReadEnable(105, session.getUserId());
			}

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			if (types) {
				// Just list the different storers (types of storages)
				Set<String> set = StorerManager.get().getDefinitions().keySet();
				for (String type : set) {
					if (!StorerManager.get().getDefinitions().get(type).isEnabled())
						continue;

					writer.print("<storage>");
					writer.print("<id>" + type + "</id>");
					writer.print("<name><![CDATA[" + I18N.message("storer." + type, ServiceUtil.currentLocale(session))
							+ "]]></name>");
					writer.print("<type>" + type + "</type>");
					writer.print("</storage>");
				}
			} else {
				// List the storages
				if ("true".equals(request.getParameter("empty"))) {
					writer.print("<storage>");
					writer.print("<id />");
					writer.print("<name />");
					writer.print("<path />");
					writer.print("<write>blank</write>");
					writer.print("<type>fs</type>");
					writer.print("</storage>");
				}

				ContextProperties conf = Context.get().getProperties();

				// Prepare the stores
				for (int i = 1; i <= 99; i++) {
					String path = conf.getProperty("store." + i + ".dir");
					if (StringUtils.isNotEmpty(path)) {
						writer.print("<storage>");
						writer.print("<id>" + i + "</id>");
						writer.print("<name>Storage " + i + "</name>");
						writer.print("<path><![CDATA[" + path + "]]></path>");
						writer.print("<write>" + (conf.getInt("store.write") == i ? "database_edit" : "blank")
								+ "</write>");
						String type = conf.getProperty("store." + i + ".type");
						if (StringUtils.isEmpty(type))
							type = "fs";
						writer.print("<type>" + type + "</type>");

						if (parameters) {
							Storer st = StorerManager.get().getDefinitions().get(type);
							if (st != null) {
								for (String name : st.getParameterNames()) {
									String value = conf.getPropertyWithSubstitutions("store." + i + "." + name, "");
									writer.print("<" + name + "><![CDATA[" + value + "]]></" + name + ">");
								}
							}
						}

						writer.print("</storage>");
					}
				}
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
