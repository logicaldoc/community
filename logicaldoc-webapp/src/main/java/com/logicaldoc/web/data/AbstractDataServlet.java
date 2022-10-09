package com.logicaldoc.web.data;

import java.io.IOException;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.web.util.ServiceUtil;
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

	public AbstractDataServlet() {

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

			int max = 100;
			if (StringUtils.isNotEmpty(request.getParameter("max")))
				max = Integer.parseInt(request.getParameter("max"));

			Session session = ServiceUtil.validateSession(request);

			service(request, response, session, max);
		} catch (Throwable e) {
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
	 * @param max tha maximum number of entries to return
	 * 
	 * @throws IOException generic error
	 * @throws PersistenceException an error on the database
	 */
	protected abstract void service(HttpServletRequest request, HttpServletResponse response, Session session, int max)
			throws PersistenceException, IOException;
}
