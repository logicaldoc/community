package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.i18n.I18N;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for document posts data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class EventsDataServlet extends AbstractDataServlet {

	private static final String LABEL_CDATA = "<label><![CDATA[";

	private static final String CLOSE_EVENT = "</event>";

	private static final String CLOSE_LABEL = "]]></label>";

	private static final String CLOSE_CODE = "</code>";

	private static final String CODE = "<code>";

	private static final String EVENT = "<event>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		boolean folder = Boolean.parseBoolean(request.getParameter("folder"));
		boolean workflow = Boolean.parseBoolean(request.getParameter("workflow"));
		boolean ai = Boolean.parseBoolean(request.getParameter("ai"));
		boolean user = Boolean.parseBoolean(request.getParameter("user"));
		boolean importfolder = Boolean.parseBoolean(request.getParameter("importfolder"));
		boolean ocr = Boolean.parseBoolean(request.getParameter("ocr"));
		boolean all = Boolean.parseBoolean(request.getParameter("all"));
		boolean webservice = Boolean.parseBoolean(request.getParameter("webservice"));

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (all) {
			writer.print(EVENT);
			writer.print(CODE + "all" + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message("allevents", locale) + CLOSE_LABEL);
			writer.print("<type></type>");
			writer.print(CLOSE_EVENT);
		}

		if (folder)
			writeFolderEvents(writer, locale);

		writeDocumentEvents(writer, locale);

		if (folder)
			writeFolderEvents(writer, locale);

		if (user)
			writeUserEvents(writer, locale);

		if (workflow)
			writeWorkflowEvents(writer, locale);

		if (importfolder)
			writeImportFolderEvents(writer, locale);

		if (ocr)
			writeOcrEvents(writer, locale);

		if (ai)
			writeAiEvents(writer, locale);

		if (webservice) {
			writer.print(EVENT);
			writer.print(CODE + "event.webservice.call" + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message("event.webservice.call", locale) + CLOSE_LABEL);
			writer.print("<type>webservice</type>");
			writer.print(CLOSE_EVENT);
		}

		writer.write("</list>");
	}

	private void writeAiEvents(PrintWriter writer, Locale locale) {
		String[] events = new String[] { "event.ai.query", "event.ai.trained" };
		for (String event : events) {
			writer.print(EVENT);
			writer.print(CODE + event + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event, locale) + CLOSE_LABEL);
			writer.print("<type>ai</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeOcrEvents(PrintWriter writer, Locale locale) {
		String[] events = new String[] { "event.ocr.success", "event.ocr.failure" };
		for (String event : events) {
			writer.print(EVENT);
			writer.print(CODE + event + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event, locale) + CLOSE_LABEL);
			writer.print("<type>ocr</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeImportFolderEvents(PrintWriter writer, Locale locale) {
		String[] events = new String[] { "event.importfolder.imported", "event.importfolder.updated",
				"event.importfolder.error" };
		for (String event : events) {
			writer.print(EVENT);
			writer.print(CODE + event + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event, locale) + CLOSE_LABEL);
			writer.print("<type>workflow</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeWorkflowEvents(PrintWriter writer, Locale locale) {
		String[] events = new String[] { "event.workflow.start", "event.workflow.end", "event.workflow.deleted",
				"event.workflow.task.start", "event.workflow.task.end", "event.workflow.task.assigned",
				"event.workflow.task.reassigned", "event.workflow.task.overdue", "event.workflow.task.note",
				"event.workflow.task.invalid", "event.workflow.docappended" };
		for (String event : events) {
			writer.print(EVENT);
			writer.print(CODE + event + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event, locale) + CLOSE_LABEL);
			writer.print("<type>workflow</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeUserEvents(PrintWriter writer, Locale locale) {
		for (UserEvent event : UserEvent.values()) {
			writer.print(EVENT);
			writer.print(CODE + event.toString() + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event.toString(), locale) + CLOSE_LABEL);
			writer.print("<type>user</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeFolderEvents(PrintWriter writer, Locale locale) {
		for (FolderEvent event : FolderEvent.values()) {
			writer.print(EVENT);
			writer.print(CODE + event.toString() + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event.toString(), locale) + CLOSE_LABEL);
			writer.print("<type>folder</type>");
			writer.print(CLOSE_EVENT);
		}
	}

	private void writeDocumentEvents(PrintWriter writer, Locale locale) {
		for (DocumentEvent event : DocumentEvent.values()) {
			writer.print(EVENT);
			writer.print(CODE + event.toString() + CLOSE_CODE);
			writer.print(LABEL_CDATA + I18N.message(event.toString(), locale) + CLOSE_LABEL);
			writer.print("<type>document</type>");
			writer.print(CLOSE_EVENT);
		}
	}
}
