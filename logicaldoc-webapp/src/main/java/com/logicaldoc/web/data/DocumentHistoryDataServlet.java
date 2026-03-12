package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for documents history data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentHistoryDataServlet extends AbstractDataServlet {

	private static final String USER_ID = "userId";

	private static final String TENANT_ID = "tenantId";

	private static final String EVENT = "event";

	private static final String DOC_ID = "docId";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws IOException, PersistenceException {

		boolean showSid = MenuDAO.get().isReadAllowed(Menu.SESSIONS, session.getUserId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder("""
 												select A.username, A.event, A.version, A.date, A.comment, A.filename, A.isNew, A.folderId, A.docId, 
 												       A.path, A.sessionId, A.userId, A.reason, A.ip, A.device, A.geolocation, A.color, A.fileVersion, 
 												       A.fileSize, A.keyLabel, A.revision, A.impersonator 
 												  from DocumentHistory A 
 												 where A.deleted = 0
                                                """);
		Map<String, Object> params = prepareQueryParams(request, query);
		List<?> records = DocumentHistoryDAO.get().findByQuery(query.toString(), params, max != null ? max : 100);

		// Used only to cache the already encountered documents when the
		// history
		// is related to a single user (for dashboard visualization)
		Set<Long> docIds = new HashSet<>();

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;
			if (request.getParameter(USER_ID) != null) {
				/*
				 * If the request contains the user specification, we report
				 * just the latest event per each document
				 */
				if (docIds.contains(cols[8]))
					continue;
				else
					docIds.add((Long) cols[8]);
			}

			printHistory(writer, cols, locale, showSid);
		}
		writer.write("</list>");
	}

	private void printHistory(PrintWriter writer, Object[] historyRecord, Locale locale, boolean showSid) {
		writer.print("<history>");
		writer.print(String.format("<user><![CDATA[%s]]></user>", historyRecord[0]));
		writer.print(String.format("<event><![CDATA[%s]]></event>", I18N.message((String) historyRecord[1], locale)));
		writer.print(String.format("<version>%s</version>", historyRecord[2]));

		DateFormat df = getDateFormat();
		writer.print(String.format("<date>%s</date>", df.format((Date) historyRecord[3])));

		writer.print(String.format("<comment><![CDATA[%s]]></comment>",
				StringUtils.defaultString((String) historyRecord[4])));
		writer.print(String.format(String.format("<filename><![CDATA[%s]]></filename>",
				StringUtils.defaultString((String) historyRecord[5]))));
		writer.print(String.format("<icon>%s</icon>",
				FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension((String) historyRecord[5])))));
		writer.print(String.format("<new>%b</new>", historyRecord[6]));
		writer.print(String.format("<folderId>%d</folderId>", historyRecord[7]));
		writer.print(String.format("<docId>%d</docId>", historyRecord[8]));
		writer.print(
				String.format("<path><![CDATA[%s]]></path>", StringUtils.defaultString((String) historyRecord[9])));
		if (showSid)
			writer.print(
					String.format("<sid><![CDATA[%s]]></sid>", StringUtils.defaultString((String) historyRecord[10])));
		writer.print(String.format("<userId>%d</userId>", historyRecord[11]));

		if (historyRecord[12] != null)
			writer.print(String.format("<reason><![CDATA[%s]]></reason>", historyRecord[12]));

		writer.print(String.format("<ip><![CDATA[%s]]></ip>", StringUtils.defaultString((String) historyRecord[13])));
		writer.print(String.format("<device><![CDATA[%s]]></device>",
				StringUtils.defaultString((String) historyRecord[14])));
		writer.print(String.format("<geolocation><![CDATA[%s]]></geolocation>",
				StringUtils.defaultString((String) historyRecord[15])));

		if (historyRecord[16] != null)
			writer.write(String.format("<color><![CDATA[%s]]></color>", (String) historyRecord[16]));

		writer.print(
				String.format("<fileVersion>%s</fileVersion>", StringUtils.defaultString((String) historyRecord[17])));
		writer.print(String.format("<fileSize>%d</fileSize>", historyRecord[18]));

		if (historyRecord[19] != null)
			writer.write(String.format("<key><![CDATA[%s]]></key>", historyRecord[19]));

		writer.write(String.format("<revision><![CDATA[%s]]></revision>",
				StringUtils.defaultString((String) historyRecord[20])));
		writer.write(String.format("<impersonator>%s</impersonator>",
				StringUtils.defaultString((String) historyRecord[21])));
		writer.print("</history>");
	}

	private Map<String, Object> prepareQueryParams(HttpServletRequest request, StringBuilder query)
			throws PersistenceException {
		Map<String, Object> params = new HashMap<>();

		if (request.getParameter(DOC_ID) != null) {
			Long docId = Long.parseLong(request.getParameter(DOC_ID));
			DocumentDAO ddao = DocumentDAO.get();
			Document doc = ddao.findDocument(docId);
			if (doc != null)
				docId = doc.getId();
			query.append(" and A.docId = :docId");
			params.put(DOC_ID, docId);
		}

		if (request.getParameter(USER_ID) != null) {
			query.append(" and A.userId = :userId");
			params.put(USER_ID, Long.parseLong(request.getParameter(USER_ID)));
		}

		if (request.getParameter(TENANT_ID) != null) {
			query.append(" and A.tenantId = :tenantId");
			params.put(TENANT_ID, Long.parseLong(request.getParameter(TENANT_ID)));
		}

		String event = request.getParameter(EVENT);
		if (event != null) {
			// avoid SQL injetion
			event = event.replaceAll("[^a-zA-Z0-9.,]", "");
			if (event.contains(",")) {
				query.append(" and A.event in (");
				query.append(Arrays.asList(event.split("\\,")).stream().map(ev -> "'%s'".formatted(ev))
						.collect(Collectors.joining(",")));
				query.append(")");
			} else {
				query.append(" and A.event = :event");
				params.put(EVENT, event);
			}
		}

		query.append(" order by A.date desc ");

		return params;
	}
}