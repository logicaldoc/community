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

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

		long userId = Long.parseLong(request.getParameter("id"));
		String event = request.getParameter("event");

		List<Object> records = executeQuery(max, userId, event);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DateFormat df = getDateFormat();

		/*
		 * Iterate over the collection of user histories
		 */
		for (Object gridRecord : records) {
			Object[] cols = (Object[]) gridRecord;

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
			if (cols[11] != null)
				writer.print("<geolocation><![CDATA[" + cols[11] + "]]></geolocation>");			
			writer.print("</history>");
		}
		writer.write("</list>");
	}

	private List<Object> executeQuery(Integer max, long userId, String event) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("userId", userId);

		StringBuilder query = new StringBuilder(
				"select A.id, A.username, A.event, A.date, A.comment, A.reason, A.sessionId, A.userId, A.ip, A.device, A.geolocation, A.keyLabel from UserHistory A where A.deleted = 0 and A.userId = :userId ");
		if (StringUtils.isNotEmpty(event)) {
			query.append(" and A.event = :event ");
			params.put("event", event);
		}
		query.append(" order by A.date desc ");

		UserHistoryDAO dao = (UserHistoryDAO) Context.get().getBean(UserHistoryDAO.class);
		return dao.findByQuery(query.toString(), params, max != null ? max : 100);
	}
}