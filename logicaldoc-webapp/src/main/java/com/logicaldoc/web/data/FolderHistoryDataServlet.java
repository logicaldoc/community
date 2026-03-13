package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

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

		StringBuilder query = new StringBuilder("""
                                                select A.username, A.event, A.date, A.comment, A.filename, A.path, A.sessionId, A.id, A.reason, A.ip, A.device, 
                                                       A.geolocation, A.userId, A.color, A.keyLabel, A.impersonator 
                                                  from FolderHistory A 
                                                 where A.deleted = 0
                                                """);
		if (request.getParameter("id") != null)
			query.append(" and A.folderId = %s".formatted(request.getParameter("id")));
		query.append(" order by A.date desc ");

		List<?> records = DocumentHistoryDAO.get().findByQuery(query.toString(), (Map<String, Object>) null,
				max != null ? max : 100);

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
		writer.print(String.format("<id>%d</id>", (Long) cols[7]));
		writer.print(String.format("<user><![CDATA[%s]]></user>", StringUtils.defaultString((String) cols[0])));
		writer.print(String.format("<event><![CDATA[%s]]></event>", I18N.message((String) cols[1], locale)));
		writer.print(String.format("<date>%s</date>", df.format((Date) cols[2])));
		writer.print(String.format("<comment><![CDATA[%s]]></comment>", StringUtils.defaultString((String) cols[3])));
		writer.print(String.format("<filename><![CDATA[%s]]></filename>", StringUtils.defaultString((String) cols[4])));

		if (cols[4] != null && !FileUtil.getExtension(cols[4].toString()).isEmpty())
			writer.print(String.format("<icon>%s</icon>",
					FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension((String) cols[4])))));
		else
			writer.print("<icon>folder</icon>");

		writer.print(String.format("<path><![CDATA[%s]]></path>", StringUtils.defaultString((String) cols[5])));

		if (showSid)
			writer.print(String.format("<sid><![CDATA[%s]]></sid>", cols[6]));

		writer.print(String.format("<reason><![CDATA[%s]]></reason>", StringUtils.defaultString((String) cols[8])));
		writer.print(String.format("<ip><![CDATA[%s]]></ip>", StringUtils.defaultString((String) cols[9])));
		writer.print(String.format("<device><![CDATA[%s]]></device>", StringUtils.defaultString((String) cols[10])));
		writer.print(String.format("<userId>%d</userId>", (Long) cols[12]));

		if (cols[11] != null)
			writer.print(String.format("<geolocation><![CDATA[%s]]></geolocation>", cols[11]));
		if (cols[13] != null)
			writer.print(String.format("<color>%s</color>", cols[13]));
		if (cols[14] != null)
			writer.print(String.format("<key><![CDATA[%s]]></key>", cols[14]));

		writer.print(String.format("<impersonator>%s</impersonator>", StringUtils.defaultString((String) cols[15])));
		writer.print("</history>");
	}
}