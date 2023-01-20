package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for messages data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessagesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		/*
		 * Execute the Query
		 */
		Context context = Context.get();
		SystemMessageDAO dao = (SystemMessageDAO) context.getBean(SystemMessageDAO.class);
		dao.deleteExpiredMessages(session.getUsername());

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Iterate over records composing the response XML document
		 */
		List<SystemMessage> unread = dao.findByRecipient(session.getUsername(), Message.TYPE_SYSTEM, 0);
		for (SystemMessage record : unread) {
			writer.print("<message>");
			writer.print("<id>" + record.getId() + "</id>");
			writer.print("<subject><![CDATA[" + record.getSubject() + "]]></subject>");
			writer.print("<priority>" + record.getPrio() + "</priority>");
			writer.print("<from><![CDATA[" + record.getAuthor() + "]]></from>");
			writer.print("<avatar><![CDATA[" + record.getAuthor() + "]]></avatar>");
			writer.print("<sent>" + df.format(record.getSentDate()) + "</sent>");
			writer.print("<read>false</read>");
			writer.print("<text><![CDATA[" + record.getMessageText() + "]]></text>");
			writer.print("</message>");
		}

		List<SystemMessage> read = dao.findByRecipient(session.getUsername(), Message.TYPE_SYSTEM, 1);
		for (SystemMessage record : read) {
			writer.print("<message>");
			writer.print("<id>" + record.getId() + "</id>");
			writer.print("<subject><![CDATA[" + record.getSubject() + "]]></subject>");
			writer.print("<priority>" + record.getPrio() + "</priority>");
			writer.print("<from><![CDATA[" + record.getAuthor() + "]]></from>");
			writer.print("<avatar><![CDATA[" + record.getAuthor() + "]]></avatar>");
			writer.print("<sent>" + df.format(record.getSentDate()) + "</sent>");
			writer.print("<read>true</read>");
			writer.print("<text><![CDATA[" + record.getMessageText() + "]]></text>");
			writer.print("</message>");
		}

		writer.write("</list>");
	}
}