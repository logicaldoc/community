package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class FolderHistoryDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		MenuDAO mDao = MenuDAO.get();
		boolean showSid = mDao.isReadAllowed(Menu.SESSIONS, session.getUserId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder(
				"select A.username, A.event, A.date, A.comment, A.filename, A.path, A.sessionId, A.id, A.reason, A.ip, A.device, A.geolocation, A.userId, A.color, A.keyLabel from FolderHistory A where A.deleted = 0 ");
		if (request.getParameter("id") != null)
			query.append(" and A.folderId=" + request.getParameter("id"));
		query.append(" order by A.date desc ");

		List<?> records = DocumentHistoryDAO.get().findByQuery(query.toString(), (Map<String, Object>) null, max != null ? max : 100);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

			printHistory(writer, cols, locale, showSid);
		}
		writer.write("</list>");
	}

	private void printHistory(PrintWriter writer, Object[] cols, Locale locale, boolean showSid) {

		DateFormat df = getDateFormat();

		writer.print("<history>");
		writer.print("<id>" + cols[7] + "</id>");
		writer.print("<user><![CDATA[" + (cols[0] == null ? "" : cols[0]) + "]]></user>");
		writer.print("<event><![CDATA[" + I18N.message((String) cols[1], locale) + "]]></event>");
		writer.print("<date>" + df.format((Date) cols[2]) + "</date>");
		writer.print("<comment><![CDATA[" + (cols[3] == null ? "" : cols[3]) + "]]></comment>");
		writer.print("<filename><![CDATA[" + (cols[4] == null ? "" : cols[4]) + "]]></filename>");

		printIcon(writer, cols);

		writer.print("<path><![CDATA[" + (cols[5] == null ? "" : cols[5]) + "]]></path>");

		printSid(writer, cols, showSid);

		writer.print("<reason><![CDATA[" + (cols[8] == null ? "" : cols[8]) + "]]></reason>");
		writer.print("<ip><![CDATA[" + (cols[9] == null ? "" : cols[9]) + "]]></ip>");
		writer.print("<device><![CDATA[" + (cols[10] == null ? "" : cols[10]) + "]]></device>");
		writer.print("<userId>" + cols[12] + "</userId>");

		printGeolocation(writer, cols);

		if (cols[13] != null)
			writer.print("<color>" + cols[13] + "</color>");
		if (cols[14] != null)
			writer.print("<key><![CDATA[" + cols[14] + "]]></key>");
		writer.print("</history>");
	}

	private void printGeolocation(PrintWriter writer, Object[] cols) {
		if (cols[11] != null)
			writer.print("<geolocation><![CDATA[" + cols[11] + "]]></geolocation>");
	}

	private void printSid(PrintWriter writer, Object[] cols, boolean showSid) {
		if (showSid)
			writer.print("<sid><![CDATA[" + (cols[6] == null ? "" : cols[6]) + "]]></sid>");
	}

	private void printIcon(PrintWriter writer, Object[] cols) {
		if (cols[4] != null && !FileUtil.getExtension(cols[4].toString()).isEmpty())
			writer.print(
					"<icon>" + FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension((String) cols[4])))
							+ "</icon>");
		else
			writer.print("<icon>folder</icon>");
	}
}