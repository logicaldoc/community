package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for stores data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.3
 */
public class StoresDataServlet extends AbstractDataServlet {

	private static final String STORE = "store.";

	private static final String CLOSE_STORE = "</store>";

	private static final String START_STORE = "<store>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		boolean types = "true".equals(request.getParameter("types"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (types) {
			// Just list the different stores(types of stores)
			Store store = Store.get();
			Set<String> set = store.getStoreDefinitions().keySet();
			for (String type : set.stream().sorted().toList()) {
				if (!store.getStoreDefinitions().get(type).isEnabled())
					continue;

				writer.print(START_STORE);
				writer.print("<id>" + type + "</id>");
				writer.print("<name><![CDATA[" + I18N.message(STORE + type, locale) + "]]></name>");
				writer.print("<type>" + type + "</type>");
				writer.print(CLOSE_STORE);
			}
		} else {
			// List the stores
			if ("true".equals(request.getParameter("empty"))) {
				writer.print(START_STORE);
				writer.print("<id />");
				writer.print("<name />");
				writer.print("<path />");
				writer.print("<type>fs</type>");
				writer.print(CLOSE_STORE);
			}

			// Prepare the stores
			printStores(writer, request, session);
		}
		writer.write("</list>");
	}

	private void printStores(PrintWriter writer, HttpServletRequest request, Session session) {
		ContextProperties conf = Context.get().getProperties();
		for (int i = 1; i <= 99; i++) {
			String path = conf.getProperty(STORE + i + ".dir");
			if (StringUtils.isEmpty(path))
				continue;

			writer.print(START_STORE);
			writer.print("<id>" + i + "</id>");
			writer.print("<name>Store " + i + "</name>");
			writer.print("<path><![CDATA[" + path + "]]></path>");
			writer.print("<write>" + Boolean.toString(conf.getInt("store.write") == i) + "</write>");
			String type = conf.getProperty(STORE + i + ".type");
			if (StringUtils.isEmpty(type))
				type = "fs";
			writer.print("<type>" + type + "</type>");

			printParameters(writer, request, session, i, type);

			writer.print(CLOSE_STORE);

		}
	}

	private void printParameters(PrintWriter writer, HttpServletRequest request, Session session, int i, String type) {
		ContextProperties conf = Context.get().getProperties();
		if (isParameters(request, session)) {
			Store st = Store.get().getStoreDefinitions().get(type);
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
			MenuDAO mDao = MenuDAO.get();
			parameters = session.getTenantId() == Tenant.DEFAULT_ID && mDao.isReadAllowed(105, session.getUserId());
		}
		return parameters;
	}
}