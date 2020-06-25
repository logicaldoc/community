package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.hsqldb.lib.StringUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

public class FolderHistoryDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FolderHistoryDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

			String locale = request.getParameter("locale");
			if (StringUtil.isEmpty(locale))
				locale = "en";

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			DocumentHistoryDAO dao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
			StringBuffer query = new StringBuffer(
					"select A.username, A.event, A.date, A.comment, A.filename, A.path, A.sessionId, A.id, A.reason from FolderHistory A where A.deleted = 0 ");
			if (request.getParameter("id") != null)
				query.append(" and A.folderId=" + request.getParameter("id"));

			query.append(" order by A.date desc ");

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			List<Object> records = (List<Object>) dao.findByQuery(query.toString(), null, null);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<history>");
				writer.print("<id>" + cols[7] + "</id>");
				writer.print("<user><![CDATA[" + (cols[0] == null ? "" : cols[0]) + "]]></user>");
				writer.print("<event><![CDATA[" + I18N.message((String) cols[1], locale) + "]]></event>");
				writer.print("<date>" + df.format((Date) cols[2]) + "</date>");
				writer.print("<comment><![CDATA[" + (cols[3] == null ? "" : cols[3]) + "]]></comment>");
				writer.print("<filename><![CDATA[" + (cols[4] == null ? "" : cols[4]) + "]]></filename>");
				writer.print("<path><![CDATA[" + (cols[5] == null ? "" : cols[5]) + "]]></path>");
				if (showSid)
					writer.print("<sid><![CDATA[" + (cols[6] == null ? "" : cols[6]) + "]]></sid>");
				writer.print("<reason><![CDATA[" + (cols[7] == null ? "" : cols[7]) + "]]></reason>");
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