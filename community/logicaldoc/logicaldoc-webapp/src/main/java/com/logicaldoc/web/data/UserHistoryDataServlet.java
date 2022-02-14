package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
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
			Integer max = request.getParameter("max") != null ? Integer.parseInt(request.getParameter("max")) : null;

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

			StringBuffer query = new StringBuffer(
					"select A.id, A.username, A.event, A.date, A.comment, A.reason, A.sessionId, A.userId, A.ip, A.device, A.geolocation from UserHistory A where A.deleted = 0 and A.userId = ?1 ");
			List<Object> parameters = new ArrayList<Object>();
			parameters.add(userId);
			if (StringUtils.isNotEmpty(event)) {
				query.append(" and A.event = ?2 ");
				parameters.add(event);
			}
			query.append(" order by A.date desc ");

			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), parameters.toArray(new Object[0]),
					max);

			/*
			 * Iterate over the collection of user histories
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<history>");
				writer.print("<id>" + cols[0] + "</id>");
				writer.print("<user><![CDATA[" + cols[1] + "]]></user>");
				writer.print("<event><![CDATA[" + I18N.message((String) cols[2], locale) + "]]></event>");
				writer.print("<date>" + df.format((Date) cols[3]) + "</date>");
				if (cols[4] != null)
					writer.print("<comment><![CDATA[" + cols[4] + "]]></comment>");
				if (cols[5] != null)
					writer.print("<reason><![CDATA[" + cols[5] + "]]></reason>");
				if (cols[6] != null && showSid)
					writer.print("<sid><![CDATA[" + cols[6] + "]]></sid>");
				writer.print("<userId>" + cols[7] + "</userId>");
				if (cols[8] != null)
					writer.print("<ip><![CDATA[" + cols[8] + "]]></ip>");
				if (cols[9] != null)
					writer.print("<device><![CDATA[" + cols[9] + "]]></device>");
				if (cols[10] != null)
					writer.print("<geolocation><![CDATA[" + cols[10] + "]]></geolocation>");
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