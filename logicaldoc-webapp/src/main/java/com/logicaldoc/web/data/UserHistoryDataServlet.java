package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for user history data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UserHistoryDataServlet extends AbstractDataServlet {

	private static final String TENANT_ID = "tenantId";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		MenuDAO mDao = Context.get(MenuDAO.class);
		boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

		Long userId = StringUtils.isNotEmpty(request.getParameter("id")) ? Long.parseLong(request.getParameter("id"))
				: null;
		Long tenantId = StringUtils.isNotEmpty(request.getParameter(TENANT_ID))
				? Long.parseLong(request.getParameter(TENANT_ID))
				: null;
		String comment = request.getParameter("comment");
		String event = request.getParameter("event");

		List<?> records = executeQuery(max, tenantId, userId, event, comment);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DateFormat df = getDateFormat();

		/*
		 * Iterate over the collection of user histories
		 */
		for (Object gridRecord : records)
			printHistory(writer, (Object[]) gridRecord, locale, showSid, df);

		writer.write("</list>");
	}

	private void printHistory(PrintWriter writer, Object[] columns, Locale locale, boolean showSid, DateFormat df) {
		writer.print("<history>");
		writer.print("<id>" + columns[0] + "</id>");
		writer.print("<user><![CDATA[" + columns[1] + "]]></user>");
		writer.print("<event><![CDATA[" + I18N.message((String) columns[2], locale) + "]]></event>");
		writer.print("<date>" + df.format((Date) columns[3]) + "</date>");
		if (columns[4] != null)
			writer.print("<comment><![CDATA[" + columns[4] + "]]></comment>");
		if (columns[5] != null && showSid)
			writer.print("<sid><![CDATA[" + columns[5] + "]]></sid>");
		writer.print("<userId>" + columns[6] + "</userId>");
		if (columns[7] != null)
			writer.print("<ip><![CDATA[" + columns[7] + "]]></ip>");
		if (columns[8] != null)
			writer.print("<device><![CDATA[" + columns[8] + "]]></device>");
		if (columns[9] != null)
			writer.print("<geolocation><![CDATA[" + columns[9] + "]]></geolocation>");
		if (columns[10] != null)
			writer.write("<key><![CDATA[" + columns[10] + "]]></key>");
		writer.print("</history>");
	}

	private List<?> executeQuery(Integer max, Long tenantId, Long userId, String event, String comment)
			throws PersistenceException {
		Map<String, Object> params = new HashMap<>();

		StringBuilder query = new StringBuilder(
				"select A.id, A.username, A.event, A.date, A.comment, A.sessionId, A.userId, A.ip, A.device, A.geolocation, A.keyLabel from UserHistory A where A.deleted = 0 ");
		if (StringUtils.isNotEmpty(event)) {
			query.append(" and A.event = :event ");
			params.put("event", event);
		}
		if (tenantId != null) {
			query.append(" and A.tenantId = :tenantId ");
			params.put(TENANT_ID, tenantId);
		}
		if (userId != null) {
			query.append(" and A.userId = :userId ");
			params.put("userId", userId);
		}
		if (StringUtils.isNotEmpty(comment)) {
			query.append(" and A.comment like :comment ");
			params.put("comment", comment + "%");
		}

		query.append(" order by A.date desc ");

		return Context.get(UserHistoryDAO.class).findByQuery(query.toString(), params,
				max != null ? max : 100);
	}
}