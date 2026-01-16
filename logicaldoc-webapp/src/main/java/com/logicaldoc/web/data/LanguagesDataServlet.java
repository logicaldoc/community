package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for users data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LanguagesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		boolean gui = Boolean.parseBoolean(request.getParameter("gui"));

		PrintWriter writer = response.getWriter();
		writer.print("<list>");

		if (gui) {
			ContextProperties pbean = Context.get().getConfig();
			List<String> installedLocales = I18N.getLocales();
			for (String loc : installedLocales) {
				Locale lc = LocaleUtil.toLocale(loc);

				writer.print("<lang>");
				writer.print("<code><![CDATA[" + loc + "]]></code>");
				writer.print("<name><![CDATA[" + StringUtils.capitalize(lc.getDisplayName(locale)) + "]]></name>");
				writer.print("<eenabled>"
						+ "enabled".equals(pbean.getProperty(session.getTenantName() + ".lang." + loc + ".gui"))
						+ "</eenabled>");
				writer.print("</lang>");
			}
		} else {
			Collection<Language> languages = LanguageManager.getInstance().getLanguages();
			Collection<Language> activeLanguages = LanguageManager.getInstance()
					.getActiveLanguages(session.getTenantName());

			for (Language language : languages) {
				writer.print("<lang>");
				writer.print("<code><![CDATA[" + language.toString() + "]]></code>");
				writer.print("<name><![CDATA[" + StringUtils.capitalize(language.getLocale().getDisplayName(locale))
						+ "]]></name>");
				writer.print("<eenabled>" + activeLanguages.contains(language) + "</eenabled>");
				writer.print("</lang>");
			}
		}
		writer.print("</list>");
	}
}
