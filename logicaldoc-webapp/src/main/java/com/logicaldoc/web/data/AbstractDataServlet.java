package com.logicaldoc.web.data;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.web.util.ServletUtil;

/**
 * An abstract implementation for data servlets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class AbstractDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(AbstractDataServlet.class);

	protected static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSXXX";
	
	
	protected AbstractDataServlet() {

	}

	protected boolean isSessionRequired() {
		return true;
	}
	
	protected DateFormat getDateFormat() {
		DateFormat df = new SimpleDateFormat(DATE_FORMAT);
		return df;
	}
	
	/**
	 * Standard implementation of the service method that perform the session
	 * checks. It does not throws exception++ to avoid DoS.
	 * 
	 * @param request the servlet request
	 * @param response the servlet response
	 * 
	 */
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		try {
			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			Integer max = null;
			if (StringUtils.isNotEmpty(request.getParameter("max")))
				max = Integer.parseInt(request.getParameter("max"));

			Locale locale = Locale.ENGLISH;
			if (StringUtils.isNotEmpty(request.getParameter("locale")))
				locale = LocaleUtil.toLocale(request.getParameter("locale"));

			Session session = isSessionRequired() ? ServletUtil.validateSession(request) : null;

			service(request, response, session, max, locale);
		} catch (NumberFormatException | PersistenceException | IOException e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	/**
	 * Concrete implementations of this method should build her the XML
	 * structure to return to the client
	 * 
	 * @param request the servlet request
	 * @param response the servlet response
	 * @param session the current session
	 * @param max optional maximum number of entries to return
	 * @param locale an optional locale specification
	 * 
	 * @throws IOException generic error
	 * @throws PersistenceException an error on the database
	 */
	protected abstract void service(HttpServletRequest request, HttpServletResponse response, Session session,
			Integer max, Locale locale) throws PersistenceException, IOException;
}
