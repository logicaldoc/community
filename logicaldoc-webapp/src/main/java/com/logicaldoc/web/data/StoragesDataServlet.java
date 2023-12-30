package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This servlet is responsible for storages data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class StoragesDataServlet extends AbstractDataServlet {

	private static final String STORE = "store.";

	private static final String CLOSE_STORAGE = "</storage>";

	private static final String STORAGE = "<storage>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		boolean types = "true".equals(request.getParameter("types"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (types) {
			// Just list the different storers (types of storages)
			Storer manager = (Storer) Context.get().getBean(Storer.class);
			Set<String> set = manager.getStorerDefinitions().keySet();
			for (String type : set.stream().sorted().collect(Collectors.toList())) {
				if (!manager.getStorerDefinitions().get(type).isEnabled())
					continue;

				writer.print(STORAGE);
				writer.print("<id>" + type + "</id>");
				writer.print("<name><![CDATA[" + I18N.message("storer." + type, locale) + "]]></name>");
				writer.print("<type>" + type + "</type>");
				writer.print(CLOSE_STORAGE);
			}
		} else {
			// List the storages
			if ("true".equals(request.getParameter("empty"))) {
				writer.print(STORAGE);
				writer.print("<id />");
				writer.print("<name />");
				writer.print("<path />");
				writer.print("<write>blank</write>");
				writer.print("<type>fs</type>");
				writer.print(CLOSE_STORAGE);
			}

			// Prepare the stores
			printStorages(writer, request, session);
		}
		writer.write("</list>");
	}

	private void printStorages(PrintWriter writer, HttpServletRequest request, Session session) {
		ContextProperties conf = Context.get().getProperties();
		for (int i = 1; i <= 99; i++) {
			String path = conf.getProperty(STORE + i + ".dir");
			if (StringUtils.isEmpty(path))
				continue;

			writer.print(STORAGE);
			writer.print("<id>" + i + "</id>");
			writer.print("<name>Storage " + i + "</name>");
			writer.print("<path><![CDATA[" + path + "]]></path>");
			writer.print("<write>" + (conf.getInt("store.write") == i ? "database_edit" : "blank") + "</write>");
			String type = conf.getProperty(STORE + i + ".type");
			if (StringUtils.isEmpty(type))
				type = "fs";
			writer.print("<type>" + type + "</type>");

			printParameters(writer, request, session, i, type);

			writer.print(CLOSE_STORAGE);

		}
	}

	private void printParameters(PrintWriter writer, HttpServletRequest request, Session session, int i, String type) {
		ContextProperties conf = Context.get().getProperties();
		if (isParameters(request, session)) {
			Storer manager = (Storer) Context.get().getBean(Storer.class);
			Storer st = manager.getStorerDefinitions().get(type);
			if (st != null) {
				for (String name : st.getParameterNames()) {
					String value = conf.getProperty(STORE + i + "." + name, "");
					writer.print("<" + name + "><![CDATA[" + value + "]]></" + name + ">");
				}
			}
		}
	}

	private boolean isParameters(HttpServletRequest request, Session session) {
		boolean parameters = "true".equals(request.getParameter("parameters"));
		if (parameters) {
			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			parameters = session.getTenantId() == Tenant.DEFAULT_ID && mDao.isReadEnable(105, session.getUserId());
		}
		return parameters;
	}
}