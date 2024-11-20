package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for templates data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ApiKeysDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		ApiKeyDAO dao = Context.get().getBean(ApiKeyDAO.class);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		List<ApiKey> apiKeys = dao.findByUser(session.getUserId());

		/*
		 * Iterate over the collection of templates
		 */
		for (ApiKey apiKey : apiKeys) {
			writer.print("<apikey>");
			writer.print("<id>" + apiKey.getId() + "</id>");
			writer.print("<name><![CDATA[" + apiKey.getName() + "]]></name>");
			writer.print("<key><![CDATA[" + apiKey.getLabel() + "]]></key>");
			writer.print("<creation>" + getDateFormat().format(apiKey.getCreation()) + "</creation>");
			if(apiKey.getLastUsed()!=null)
				writer.print("<lastUsed>" + getDateFormat().format(apiKey.getLastUsed()) + "</lastUsed>");
			writer.print("</apikey>");
		}

		writer.write("</list>");
	}
}