package com.logicaldoc.core.dashlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;

/**
 * This servlet is responsible for rendering dashlet's contents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletContent extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DashletContent.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = validateSession(request);

			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean showSid = mDao.isReadEnable(Menu.SESSIONS, session.getUserId());

			Locale locale = LocaleUtil.toLocale(request.getParameter("locale"));
			long dashletId = Long.parseLong(request.getParameter("dashletId"));

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);
			response.setCharacterEncoding("UTF-8");

			DashletDAO dao = (DashletDAO) Context.get().getBean(DashletDAO.class);
			Dashlet dashlet = dao.findById(dashletId);
			if (Dashlet.TYPE_CONTENT.equals(dashlet.getType()))
				response.setContentType("text/html");
			else
				response.setContentType("text/xml");

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			Map<String, Object> dashletDictionary = new HashMap<String, Object>();
			dashletDictionary.put(Automation.LOCALE, locale);
			dashletDictionary.put(Automation.TENANT_ID, dashlet.getTenantId());
			dashletDictionary.put("session", session);
			dashletDictionary.put("user", session.getUser());
			dashletDictionary.put("dashlet", dashlet);

			Automation automation = new Automation("dashlet-" + dashlet.getName());
			PrintWriter writer = response.getWriter();

			if (Dashlet.TYPE_DOCEVENT.equals(dashlet.getType()))
				handleDocEvent(showSid, locale, dashlet, df, dashletDictionary, automation, writer);
			else if (Dashlet.TYPE_DOCUMENT.equals(dashlet.getType()))
				handleDocument(showSid, locale, dashlet, df, dashletDictionary, automation, writer);
			else if (Dashlet.TYPE_NOTE.equals(dashlet.getType()))
				handleNote(showSid, locale, dashlet, df, dashletDictionary, automation, writer);
			else if (Dashlet.TYPE_CONTENT.equals(dashlet.getType()))
				handleContent(dashlet, dashletDictionary, automation, writer);
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

	private void handleDocEvent(boolean showSid, Locale locale, Dashlet dashlet, DateFormat df,
			Map<String, Object> dashletDictionary, Automation automation, PrintWriter writer) {
		if (StringUtils.isNotEmpty(dashlet.getContent())) {
			String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
			if (StringUtils.isNotEmpty(content))
				writer.write(content.trim());
		} else {
			writer.write("<list>");

			DocumentHistoryDAO hdao = (DocumentHistoryDAO) Context.get().getBean(DocumentHistoryDAO.class);
			String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);
			List<DocumentHistory> records = new ArrayList<DocumentHistory>();

			try {
				records = hdao.findByObjectQuery(query.trim(), null, dashlet.getMax());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			/*
			 * Iterate over records composing the response XML document
			 */
			for (DocumentHistory record : records) {
				writer.write("<history>");
				writer.write("<user><![CDATA[" + record.getUsername() + "]]></user>");
				writer.write("<event><![CDATA[" + I18N.message(record.getEvent(), locale) + "]]></event>");
				writer.write("<version>" + record.getVersion() + "</version>");
				writer.write("<date>" + df.format(record.getDate()) + "</date>");
				writer.write("<comment><![CDATA[" + (record.getComment() == null ? "" : record.getComment())
						+ "]]></comment>");
				writer.write("<filename><![CDATA[" + (record.getFilename() == null ? "" : record.getFilename())
						+ "]]></filename>");
				writer.write("<icon>"
						+ FilenameUtils.getBaseName(
								IconSelector.selectIcon(FilenameUtils.getExtension((String) record.getFilename())))
						+ "</icon>");
				writer.write("<new>" + (1 == record.getIsNew()) + "</new>");
				writer.write("<folderId>" + record.getFolderId() + "</folderId>");
				writer.write("<docId>" + record.getDocId() + "</docId>");
				writer.write("<path><![CDATA[" + (record.getPath() == null ? "" : record.getPath()) + "]]></path>");
				if (showSid)
					writer.write("<sid><![CDATA[" + (record.getSessionId() == null ? "" : record.getSessionId())
							+ "]]></sid>");
				writer.write("<userid>" + record.getUserId() + "</userid>");
				writer.write(
						"<reason><![CDATA[" + (record.getReason() == null ? "" : record.getReason()) + "]]></reason>");
				writer.write("</history>");
			}
			writer.write("</list>");
		}
	}

	private void handleDocument(boolean showSid, Locale locale, Dashlet dashlet, DateFormat df,
			Map<String, Object> dashletDictionary, Automation automation, PrintWriter writer) {
		if (StringUtils.isNotEmpty(dashlet.getContent())) {
			String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
			if (StringUtils.isNotEmpty(content))
				writer.write(content.trim());
		} else {
			writer.write("<list>");

			DocumentDAO hdao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);

			List<Document> records = new ArrayList<Document>();

			try {
				records = hdao.findByObjectQuery(query.trim(), null, dashlet.getMax());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Document record : records) {
				writer.write("<document>");
				writer.write("<id>" + record.getId() + "</id>");
				if (record.getFolder() != null)
					writer.write("<folderId>" + record.getFolder().getId() + "</folderId>");
				writer.write("<customId><![CDATA[" + (record.getCustomId() != null ? record.getCustomId() : "")
						+ "]]></customId>");
				if (record.getDocRef() != null) {
					writer.write("<docref>" + record.getDocRef() + "</docref>");
					if (record.getDocRefType() != null)
						writer.write("<docrefType>" + record.getDocRefType() + "</docrefType>");
				}

				writer.write("<icon>" + FilenameUtils.getBaseName(record.getIcon()) + "</icon>");
				writer.write("<version>" + record.getVersion() + "</version>");
				writer.write(
						"<lastModified>" + (record.getLastModified() != null ? df.format(record.getLastModified()) : "")
								+ "</lastModified>");
				writer.write(
						"<published>" + (record.getDate() != null ? df.format(record.getDate()) : "") + "</published>");
				writer.write("<publisher><![CDATA[" + record.getPublisher() + "]]></publisher>");
				writer.write("<created>" + (record.getCreation() != null ? df.format(record.getCreation()) : "")
						+ "</created>");
				writer.write("<creator><![CDATA[" + record.getCreator() + "]]></creator>");
				writer.write("<size>" + record.getFileSize() + "</size>");

				writer.write("<status>" + record.getStatus() + "</status>");
				writer.write("<immutable>" + record.getImmutable() + "</immutable>");
				writer.write("<indexed>" + record.getIndexed() + "</indexed>");
				writer.write("<password>" + StringUtils.isNotEmpty(record.getPassword()) + "</password>");
				writer.write("<signed>" + record.getSigned() + "</signed>");
				writer.write("<stamped>" + record.getStamped() + "</stamped>");

				if (record.getLockUserId() != null)
					writer.write("<lockUserId>" + record.getLockUserId() + "</lockUserId>");
				if (record.getLockUser() != null)
					writer.write("<lockUser><![CDATA[" + record.getLockUser() + "]]></lockUser>");
				writer.write("<filename><![CDATA[" + record.getFileName() + "]]></filename>");
				writer.write("<type><![CDATA[" + record.getType() + "]]></type>");

				writer.write("<rating>" + (record.getRating() != null ? record.getRating() : "0") + "</rating>");
				writer.write("<fileVersion><![CDATA[" + record.getFileVersion() + "]]></fileVersion>");
				writer.write("<comment><![CDATA[" + (record.getComment() != null ? record.getComment() : "")
						+ "]]></comment>");
				writer.write("<workflowStatus><![CDATA["
						+ (record.getWorkflowStatus() != null ? record.getWorkflowStatus() : "")
						+ "]]></workflowStatus>");
				writer.write("<workflowStatusDisplay><![CDATA["
						+ (record.getWorkflowStatusDisplay() != null ? record.getWorkflowStatusDisplay() : "")
						+ "]]></workflowStatusDisplay>");
				if (record.getStartPublishing() != null)
					writer.write("<startPublishing>" + df.format(record.getStartPublishing()) + "</startPublishing>");
				else
					writer.write("<startPublishing></startPublishing>");
				if (record.getStopPublishing() != null)
					writer.write("<stopPublishing>" + df.format(record.getStopPublishing()) + "</stopPublishing>");
				else
					writer.write("<stopPublishing></stopPublishing>");
				writer.write("<publishedStatus>" + (record.isPublishing() ? "yes" : "no") + "</publishedStatus>");

				if (record.getExtResId() != null)
					writer.write("<extResId><![CDATA[" + record.getExtResId() + "]]></extResId>");

				if (record.getTemplateName() != null)
					writer.write("<template><![CDATA[" + record.getTemplateName() + "]]></template>");

				writer.write("</document>");
			}

			writer.write("</list>");
		}
	}

	private void handleNote(boolean showSid, Locale locale, Dashlet dashlet, DateFormat df,
			Map<String, Object> dashletDictionary, Automation automation, PrintWriter writer) {

		if (StringUtils.isNotEmpty(dashlet.getContent())) {
			String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
			if (StringUtils.isNotEmpty(content))
				writer.write(content.trim());
		} else {
			writer.write("<list>");

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);
			List<DocumentNote> records = new ArrayList<DocumentNote>();
			try {
				records = dao.findByObjectQuery(query.trim(), null, dashlet.getMax());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			/*
			 * Iterate over records composing the response XML document
			 */
			for (DocumentNote record : records) {
				writer.write("<post>");
				writer.write("<id>" + record.getId() + "</id>");
				writer.write("<title><![CDATA[" + StringUtils.abbreviate(record.getMessage(), 100) + "]]></title>");
				writer.write("<page>" + record.getPage() + "</page>");
				writer.write("<user><![CDATA[" + record.getUsername() + "]]></user>");
				writer.write("<date>" + (record.getDate() != null ? df.format(record.getDate()) : "") + "</date>");
				writer.write("<message><![CDATA[" + record.getMessage() + "]]></message>");
				writer.write("<docId>" + record.getDocId() + "</docId>");
				writer.write("<filename><![CDATA[" + record.getFileName() + "]]></filename>");
				writer.write("<userId><![CDATA[" + record.getUserId() + "]]></userId>");
				writer.write("</post>");
			}

			writer.write("</list>");
		}
	}

	private void handleContent(Dashlet dashlet, Map<String, Object> dashletDictionary, Automation automation,
			PrintWriter writer) {
		String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
		if (StringUtils.isNotEmpty(content))
			writer.write(content.trim());
	}

	public static Session validateSession(HttpServletRequest request) throws ServletException {
		String sid = SessionManager.get().getSessionId(request);
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new ServletException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new ServletException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}
}