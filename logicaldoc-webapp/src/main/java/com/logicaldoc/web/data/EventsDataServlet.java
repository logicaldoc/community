package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.i18n.I18N;

/**
 * This servlet is responsible for document posts data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class EventsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max, Locale locale)
			throws PersistenceException, IOException {

		boolean folder = Boolean.parseBoolean(request.getParameter("folder"));
		boolean workflow = Boolean.parseBoolean(request.getParameter("workflow"));
		boolean user = Boolean.parseBoolean(request.getParameter("user"));
		boolean importfolder = Boolean.parseBoolean(request.getParameter("importfolder"));

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		// Avoid resource caching
		response.setHeader("Pragma", "no-cache");
		response.setHeader("Cache-Control", "no-store");
		response.setDateHeader("Expires", 0);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		for (DocumentEvent event : DocumentEvent.values()) {
			writer.print("<event>");
			writer.print("<code>" + event.toString() + "</code>");
			writer.print("<label><![CDATA[" + I18N.message(event.toString(), locale) + "]]></label>");
			writer.print("<type>document</type>");
			writer.print("</event>");
		}

		if (folder)
			for (FolderEvent event : FolderEvent.values()) {
				writer.print("<event>");
				writer.print("<code>" + event.toString() + "</code>");
				writer.print("<label><![CDATA[" + I18N.message(event.toString(), locale) + "]]></label>");
				writer.print("<type>folder</type>");
				writer.print("</event>");
			}

		if (user) {
			for (UserEvent event : UserEvent.values()) {
				writer.print("<event>");
				writer.print("<code>" + event.toString() + "</code>");
				writer.print("<label><![CDATA[" + I18N.message(event.toString(), locale) + "]]></label>");
				writer.print("<type>user</type>");
				writer.print("</event>");
			}
		}

		if (workflow) {
			String[] events = new String[] { "event.workflow.start", "event.workflow.end", "event.workflow.deleted",
					"event.workflow.task.start", "event.workflow.task.end", "event.workflow.task.assigned",
					"event.workflow.docappended", "event.workflow.task.reassigned", "event.workflow.task.note" };
			for (String event : events) {
				writer.print("<event>");
				writer.print("<code>" + event + "</code>");
				writer.print("<label><![CDATA[" + I18N.message(event, locale) + "]]></label>");
				writer.print("<type>workflow</type>");
				writer.print("</event>");
			}
		}

		if (importfolder) {
			String[] events = new String[] { "event.importfolder.imported", "event.importfolder.updated",
					"event.importfolder.error" };
			for (String event : events) {
				writer.print("<event>");
				writer.print("<code>" + event + "</code>");
				writer.print("<label><![CDATA[" + I18N.message(event, locale) + "]]></label>");
				writer.print("<type>workflow</type>");
				writer.print("</event>");
			}
		}

		writer.write("</list>");
	}
}
