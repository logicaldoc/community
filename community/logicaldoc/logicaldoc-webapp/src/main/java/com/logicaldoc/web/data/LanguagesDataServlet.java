package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for users data.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class LanguagesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(LanguagesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			String locale = request.getParameter("locale");
			boolean gui = Boolean.parseBoolean(request.getParameter("gui"));

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.print("<list>");

			if (gui) {
				ContextProperties pbean = Context.get().getProperties();
				List<String> installedLocales = I18N.getLocales();
				for (String loc : installedLocales) {
					Locale lc = LocaleUtil.toLocale(loc);

					writer.print("<lang>");
					writer.print("<code><![CDATA[" + loc + "]]></code>");
					writer.print("<name><![CDATA["
							+ StringUtils.capitalize(lc.getDisplayName(LocaleUtil.toLocale(locale))) + "]]></name>");
					if ("enabled".equals(pbean.getProperty(session.getTenantName()+".lang." + loc + ".gui")))
						writer.print("<eenabled>0</eenabled>");
					else
						writer.print("<eenabled>2</eenabled>");
					writer.print("</lang>");
				}
			} else {
				Collection<Language> languages = LanguageManager.getInstance().getLanguages();
				Collection<Language> activeLanguages = LanguageManager.getInstance().getActiveLanguages(session.getTenantName());

				for (Language language : languages) {
					writer.print("<lang>");
					writer.print("<code><![CDATA[" + language.toString() + "]]></code>");
					writer.print("<name><![CDATA["
							+ StringUtils.capitalize(language.getLocale().getDisplayName(LocaleUtil.toLocale(locale)))
							+ "]]></name>");
					if (activeLanguages.contains(language))
						writer.print("<eenabled>0</eenabled>");
					else
						writer.print("<eenabled>2</eenabled>");
					writer.print("</lang>");
				}
			}
			writer.print("</list>");
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
