package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.UserHistory;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserHistoryDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for user history data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UserHistoryDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(UserHistoryDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

			long userId = Long.parseLong(request.getParameter("id"));
			String event = request.getParameter("event");
			String locale = request.getParameter("locale");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			UserHistoryDAO dao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			/*
			 * Iterate over the collection of user histories
			 */
			for (UserHistory history : dao.findByUserIdAndEvent(userId, event)) {
				writer.print("<history>");
				writer.print("<id>" + history.getId() + "</id>");
				writer.print("<user><![CDATA[" + history.getUsername() + "]]></user>");
				writer.print("<event><![CDATA[" + I18N.message(history.getEvent(), locale) + "]]></event>");
				writer.print("<date>" + df.format(history.getDate()) + "</date>");
				if (history.getComment() != null)
					writer.print("<comment><![CDATA[" + history.getComment() + "]]></comment>");
				if (history.getReason() != null)
					writer.print("<reason><![CDATA[" + history.getReason() + "]]></reason>");
				if (history.getSessionId() != null && showSid)
					writer.print("<sid><![CDATA[" + history.getSessionId() + "]]></sid>");
				writer.print("<userId>" + history.getUserId() + "</userId>");
				if (history.getIp() != null)
					writer.print("<ip><![CDATA[" + history.getIp() + "]]></ip>");
				if (history.getDevice() != null)
					writer.print("<device><![CDATA[" + history.getDevice() + "]]></device>");
				if (history.getGeolocation() != null)
					writer.print("<geolocation><![CDATA[" + history.getGeolocation() + "]]></geolocation>");
				writer.print("</history>");
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