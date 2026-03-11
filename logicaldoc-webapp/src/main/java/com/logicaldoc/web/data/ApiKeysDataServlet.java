package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		ApiKeyDAO dao = ApiKeyDAO.get();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		List<ApiKey> apiKeys = dao.findByUser(session.getUserId());

		/*
		 * Iterate over the collection of templates
		 */
		for (ApiKey apiKey : apiKeys) {
			writer.print("<apikey>");
			writer.print(String.format("<id>%d</id>", apiKey.getId()));
			writer.print(String.format("<name><![CDATA[%s]]></name>", apiKey.getName()));
			writer.print(String.format("<key><![CDATA[%s]]></key>", apiKey.getLabel()));
			writer.print(String.format("<creation>%s</creation>", getDateFormat().format(apiKey.getCreation())));
			if (apiKey.getLastUsed() != null)
				writer.print(String.format("<lastUsed>%s</lastUsed>", getDateFormat().format(apiKey.getLastUsed())));
			writer.print("</apikey>");
		}

		writer.write("</list>");
	}
}