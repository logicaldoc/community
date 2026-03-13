package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
		SystemMessageDAO dao = SystemMessageDAO.get();
		dao.deleteExpiredMessages(session.getUsername());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Iterate over records composing the response XML document
		 */
		List<SystemMessage> unread = dao.findByRecipient(session.getUsername(), Message.TYPE_SYSTEM, 0);
		for (SystemMessage message : unread)
			printMessage(writer, message, false);

		List<SystemMessage> read = dao.findByRecipient(session.getUsername(), Message.TYPE_SYSTEM, 1);
		for (SystemMessage message : read)
			printMessage(writer, message, true);

		writer.write("</list>");
	}

	private void printMessage(PrintWriter writer, SystemMessage message, boolean read) {
		writer.print("<message>");
		writer.print(String.format("<id>%d</id>", message.getId()));
		writer.print(String.format("<subject><![CDATA[%s]]></subject>", message.getSubject()));
		writer.print(String.format("<priority>%d</priority>", message.getPrio()));
		writer.print(String.format("<from><![CDATA[%s]]></from>", message.getAuthor()));
		writer.print(String.format("<avatar><![CDATA[%s]]></avatar>", message.getAuthor()));
		writer.print(String.format("<sent>%s</sent>", getDateFormat().format(message.getSentDate())));
		writer.print(String.format("<read>%b</read>", read));
		writer.print(String.format("<text><![CDATA[%s]]></text>", message.getMessageText()));
		writer.print("</message>");
	}
}