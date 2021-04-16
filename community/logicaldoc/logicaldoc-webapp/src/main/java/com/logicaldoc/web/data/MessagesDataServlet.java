package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for messages data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessagesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(MessagesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

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
			List<SystemMessage> unread = dao.findByRecipient(session.getUsername(), SystemMessage.TYPE_SYSTEM, 0);
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

			List<SystemMessage> read = dao.findByRecipient(session.getUsername(), SystemMessage.TYPE_SYSTEM, 1);
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