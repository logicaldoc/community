package com.logicaldoc.core.dashlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentHistoryDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.FileUtil;

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
			Map<String, Object> dashletDictionary, Automation automation, PrintWriter writer)
			throws PersistenceException {
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
				records = hdao.findByObjectQuery(query.trim(), (Map<String, Object>) null,
						dashlet.getUnique() == 0 ? dashlet.getMax() : null);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			List<DocumentHistory> uniqueRecords = new ArrayList<DocumentHistory>();
			if (dashlet.getUnique() == 1) {
				log.debug("Ensure records uniqueness for query {}", query.trim());

				/*
				 * Make sure to have just one entry per document
				 */
				Set<Long> docIds = new HashSet<Long>();
				for (DocumentHistory history : records) {
					if (!docIds.contains(history.getDocId())) {
						docIds.add(history.getDocId());
						uniqueRecords.add(history);
					}
					if (dashlet.getMax() != null && dashlet.getMax() > 0 && uniqueRecords.size() >= dashlet.getMax())
						break;
				}

				log.debug("retrieved {} unique records", uniqueRecords.size());
			} else
				uniqueRecords = records;

			/*
			 * Retrieve documents the histories refer to
			 */
			Map<Long, Document> docsMap = new HashMap<Long, Document>();
			if (!uniqueRecords.isEmpty()) {
				String docIds = uniqueRecords.stream().map(h -> Long.toString(h.getDocId()))
						.collect(Collectors.joining(","));
				DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				try {
					List<Document> docs = ddao.findByObjectQuery("from Document where id in (" + docIds + ")",
							(Map<String, Object>) null, null);
					for (Document document : docs)
						docsMap.put(document.getId(), document);
				} catch (PersistenceException e) {
					log.error(e.getMessage(), e);
				}
			}

			/*
			 * Iterate over records composing the response XML document
			 */
			DateFormat df2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
			df2.setTimeZone(TimeZone.getTimeZone("UTC"));
			for (DocumentHistory history : uniqueRecords) {
				writer.write("<document>");
				writer.write("<id>" + history.getId() + "</id>");
				writer.write("<user><![CDATA[" + history.getUsername() + "]]></user>");
				writer.write("<event><![CDATA[" + I18N.message(history.getEvent(), locale) + "]]></event>");
				writer.write("<version>" + history.getVersion() + "</version>");
				writer.write("<date>" + df2.format(history.getDate()) + "</date>");
				writer.write("<comment><![CDATA[" + (history.getComment() == null ? "" : history.getComment())
						+ "]]></comment>");
				writer.write("<filename><![CDATA[" + (history.getFilename() == null ? "" : history.getFilename())
						+ "]]></filename>");
				writer.write("<icon>"
						+ FilenameUtils.getBaseName(
								IconSelector.selectIcon(FileUtil.getExtension((String) history.getFilename())))
						+ "</icon>");
				writer.write("<new>" + (1 == history.getIsNew()) + "</new>");
				writer.write("<folderId>" + history.getFolderId() + "</folderId>");
				writer.write("<docId>" + history.getDocId() + "</docId>");
				writer.write("<path><![CDATA[" + (history.getPath() == null ? "" : history.getPath()) + "]]></path>");
				if (showSid)
					writer.write("<sid><![CDATA[" + (history.getSessionId() == null ? "" : history.getSessionId())
							+ "]]></sid>");
				writer.write("<userid>" + history.getUserId() + "</userid>");
				writer.write("<reason><![CDATA[" + (history.getReason() == null ? "" : history.getReason())
						+ "]]></reason>");
				if (history.getColor() != null)
					writer.write("<color><![CDATA[" + history.getColor() + "]]></color>");

				Document doc = docsMap.get(history.getDocId());
				if (doc != null) {
					DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

					List<String> attrs = new ArrayList<String>();
					if (StringUtils.isNotEmpty(dashlet.getColumns())) {
						StringTokenizer st = new StringTokenizer(dashlet.getColumns().trim(), ",;");
						while (st.hasMoreElements()) {
							String token = st.nextToken().trim();
							if (token.startsWith("ext_"))
								attrs.add(token.substring(4));
						}
					}

					/*
					 * Contains the extended attributes of the documents. The
					 * key is documentId-atttributeName, the value is the
					 * attribute value. This fieldsMap is used to maximize the
					 * listing performances.
					 */
					final Map<String, Object> extValues = new HashMap<String, Object>();

					if (!attrs.isEmpty()) {
						log.debug("Search for extended attributes {}", attrs);

						StringBuffer qry = new StringBuffer(
								"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
						qry.append(" from ld_document_ext where ld_docid in (");
						qry.append(uniqueRecords.stream().map(d -> Long.toString(d.getDocId()))
								.collect(Collectors.joining(",")));
						qry.append(") and ld_name in ");
						qry.append(attrs.toString().replaceAll("\\[", "('").replaceAll("\\]", "')")
								.replaceAll(",", "','").replaceAll(" ", ""));

						dao.query(qry.toString(), null, new RowMapper<Long>() {
							@Override
							public Long mapRow(ResultSet rs, int row) throws SQLException {
								Long docId = rs.getLong(1);
								String name = rs.getString(2);
								int type = rs.getInt(3);

								String key = docId + "-" + name;

								if (type == Attribute.TYPE_STRING) {
									if (StringUtils.isNotEmpty(rs.getString(8)))
										extValues.put(key, rs.getString(8));
									else
										extValues.put(key, rs.getString(4));
								} else if (type == Attribute.TYPE_INT) {
									extValues.put(key, rs.getLong(5));
								} else if (type == Attribute.TYPE_DOUBLE) {
									extValues.put(key, rs.getDouble(6));
								} else if (type == Attribute.TYPE_DATE) {
									extValues.put(key, rs.getTimestamp(7));
								} else if (type == Attribute.TYPE_USER || type == Attribute.TYPE_FOLDER) {
									extValues.put(key, rs.getString(4));
								} else if (type == Attribute.TYPE_BOOLEAN) {
									extValues.put(key, rs.getLong(5) == 1L ? I18N.message("true", locale)
											: I18N.message("false", locale));
								}

								return null;
							}
						}, null);
					}

					writer.write("<customId><![CDATA[" + (doc.getCustomId() != null ? doc.getCustomId() : "")
							+ "]]></customId>");
					if (doc.getDocRef() != null) {
						writer.write("<docref>" + doc.getDocRef() + "</docref>");
						if (doc.getDocRefType() != null)
							writer.write("<docrefType>" + doc.getDocRefType() + "</docrefType>");
					}
					writer.write("<lastModified>"
							+ (history.getLastModified() != null ? df.format(history.getLastModified()) : "")
							+ "</lastModified>");
					writer.write("<published>" + (history.getDate() != null ? df.format(history.getDate()) : "")
							+ "</published>");
					writer.write("<publisher><![CDATA[" + doc.getPublisher() + "]]></publisher>");
					writer.write("<created>" + (doc.getCreation() != null ? df.format(doc.getCreation()) : "")
							+ "</created>");
					writer.write("<creator><![CDATA[" + doc.getCreator() + "]]></creator>");
					writer.write("<size>" + doc.getFileSize() + "</size>");

					writer.write("<status>" + doc.getStatus() + "</status>");
					writer.write("<immutable>" + doc.getImmutable() + "</immutable>");
					writer.write("<indexed>" + doc.getIndexed() + "</indexed>");
					writer.write("<password>" + StringUtils.isNotEmpty(doc.getPassword()) + "</password>");
					writer.write("<signed>" + doc.getSigned() + "</signed>");
					writer.write("<stamped>" + doc.getStamped() + "</stamped>");

					writer.write("<pages>" + doc.getPages() + "</pages>");

					if (doc.getLockUserId() != null)
						writer.write("<lockUserId>" + doc.getLockUserId() + "</lockUserId>");
					if (doc.getLockUser() != null)
						writer.write("<lockUser><![CDATA[" + doc.getLockUser() + "]]></lockUser>");
					writer.write("<type><![CDATA[" + doc.getType() + "]]></type>");

					writer.write("<fileVersion><![CDATA[" + doc.getFileVersion() + "]]></fileVersion>");
					writer.write("<rating>" + (doc.getRating() != null ? doc.getRating() : "0") + "</rating>");
					writer.write("<workflowStatus><![CDATA["
							+ (doc.getWorkflowStatus() != null ? doc.getWorkflowStatus() : "")
							+ "]]></workflowStatus>");
					writer.write("<workflowStatusDisplay><![CDATA["
							+ (doc.getWorkflowStatusDisplay() != null ? doc.getWorkflowStatusDisplay() : "")
							+ "]]></workflowStatusDisplay>");
					if (doc.getStartPublishing() != null)
						writer.write("<startPublishing>" + df.format(doc.getStartPublishing()) + "</startPublishing>");
					else
						writer.write("<startPublishing></startPublishing>");
					if (doc.getStopPublishing() != null)
						writer.write("<stopPublishing>" + df.format(doc.getStopPublishing()) + "</stopPublishing>");
					else
						writer.write("<stopPublishing></stopPublishing>");
					writer.write("<publishedStatus>" + (doc.isPublishing() ? "yes" : "no") + "</publishedStatus>");

					if (doc.getExtResId() != null)
						writer.write("<extResId><![CDATA[" + doc.getExtResId() + "]]></extResId>");

					if (doc.getTemplate() != null)
						writer.write("<template><![CDATA[" + doc.getTemplate().getName() + "]]></template>");

					for (String name : attrs) {
						String key = doc.getId() + "-" + name;
						Object val = extValues.get(key);
						if (val != null) {
							writer.print("<ext_" + name + ">");
							if (val instanceof Date)
								writer.print(df.format((Date) val));
							else if (val instanceof Integer)
								writer.print(Integer.toString((Integer) val));
							else if (val instanceof Long)
								writer.print(Long.toString((Long) val));
							else if (val instanceof Double)
								writer.print(Double.toString((Double) val));
							else
								writer.print("<![CDATA[" + val + "]]>");
							writer.print("</ext_" + name + ">");
						}
					}
				}

				writer.write("</document>");
			}
			writer.write("</list>");
		}
	}

	private void handleDocument(boolean showSid, Locale locale, Dashlet dashlet, DateFormat df,
			Map<String, Object> dashletDictionary, Automation automation, PrintWriter writer)
			throws PersistenceException {
		if (StringUtils.isNotEmpty(dashlet.getContent())) {
			String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
			if (StringUtils.isNotEmpty(content))
				writer.write(content.trim());
		} else {
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);

			List<Document> records = new ArrayList<Document>();

			try {
				records = dao.findByObjectQuery(query.trim(), (Map<String, Object>) null, dashlet.getMax());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			List<Document> uniqueRecords = new ArrayList<Document>();
			if (dashlet.getUnique() == 1) {
				log.debug("Ensure records uniqueness for query {}", query.trim());

				/*
				 * Make sure to have just one entry per document
				 */
				Set<Long> docIds = new HashSet<Long>();
				for (Document doc : records) {
					if (!docIds.contains(doc.getId())) {
						docIds.add(doc.getId());
						uniqueRecords.add(doc);
					}
					if (dashlet.getMax() != null && dashlet.getMax() > 0 && uniqueRecords.size() >= dashlet.getMax())
						break;
				}

				log.debug("retrieved {} unique records", uniqueRecords.size());
			} else
				uniqueRecords = records;

			List<String> attrs = new ArrayList<String>();
			if (StringUtils.isNotEmpty(dashlet.getColumns())) {
				StringTokenizer st = new StringTokenizer(dashlet.getColumns().trim(), ",;");
				while (st.hasMoreElements()) {
					String token = st.nextToken().trim();
					if (token.startsWith("ext_"))
						attrs.add(token.substring(4));
				}
			}

			/*
			 * Contains the extended attributes of the documents. The key is
			 * documentId-atttributeName, the value is the attribute value. This
			 * fieldsMap is used to maximize the listing performances.
			 */
			final Map<String, Object> extValues = new HashMap<String, Object>();

			if (!attrs.isEmpty()) {
				log.debug("Search for extended attributes {}", attrs);

				StringBuffer qry = new StringBuffer(
						"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
				qry.append(" from ld_document_ext where ld_docid in (");
				qry.append(uniqueRecords.stream().map(d -> Long.toString(d.getId())).collect(Collectors.joining(",")));
				qry.append(") and ld_name in ");
				qry.append(attrs.toString().replaceAll("\\[", "('").replaceAll("\\]", "')").replaceAll(",", "','")
						.replaceAll(" ", ""));

				dao.query(qry.toString(), null, new RowMapper<Long>() {
					@Override
					public Long mapRow(ResultSet rs, int row) throws SQLException {
						Long docId = rs.getLong(1);
						String name = rs.getString(2);
						int type = rs.getInt(3);

						String key = docId + "-" + name;

						if (type == Attribute.TYPE_STRING) {
							if (StringUtils.isNotEmpty(rs.getString(8)))
								extValues.put(key, rs.getString(8));
							else
								extValues.put(key, rs.getString(4));
						} else if (type == Attribute.TYPE_INT) {
							extValues.put(key, rs.getLong(5));
						} else if (type == Attribute.TYPE_DOUBLE) {
							extValues.put(key, rs.getDouble(6));
						} else if (type == Attribute.TYPE_DATE) {
							extValues.put(key, rs.getTimestamp(7));
						} else if (type == Attribute.TYPE_USER || type == Attribute.TYPE_FOLDER) {
							extValues.put(key, rs.getString(4));
						} else if (type == Attribute.TYPE_BOOLEAN) {
							extValues.put(key,
									rs.getLong(5) == 1L ? I18N.message("true", locale) : I18N.message("false", locale));
						}

						return null;
					}
				}, null);
			}

			writer.write("<list>");

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Document doc : uniqueRecords) {
				writer.write("<document>");
				writer.write("<id>" + doc.getId() + "</id>");
				writer.write("<customId><![CDATA[" + (doc.getCustomId() != null ? doc.getCustomId() : "")
						+ "]]></customId>");
				if (doc.getDocRef() != null) {
					writer.write("<docref>" + doc.getDocRef() + "</docref>");
					if (doc.getDocRefType() != null)
						writer.write("<docrefType>" + doc.getDocRefType() + "</docrefType>");
				}

				writer.write("<version>" + doc.getVersion() + "</version>");
				writer.write("<lastModified>" + (doc.getLastModified() != null ? df.format(doc.getLastModified()) : "")
						+ "</lastModified>");
				writer.write("<published>" + (doc.getDate() != null ? df.format(doc.getDate()) : "") + "</published>");
				writer.write("<publisher><![CDATA[" + doc.getPublisher() + "]]></publisher>");
				writer.write(
						"<created>" + (doc.getCreation() != null ? df.format(doc.getCreation()) : "") + "</created>");
				writer.write("<creator><![CDATA[" + doc.getCreator() + "]]></creator>");
				writer.write("<size>" + doc.getFileSize() + "</size>");

				writer.write("<status>" + doc.getStatus() + "</status>");
				writer.write("<immutable>" + doc.getImmutable() + "</immutable>");
				writer.write("<indexed>" + doc.getIndexed() + "</indexed>");
				writer.write("<password>" + StringUtils.isNotEmpty(doc.getPassword()) + "</password>");
				writer.write("<signed>" + doc.getSigned() + "</signed>");
				writer.write("<stamped>" + doc.getStamped() + "</stamped>");

				writer.write("<pages>" + doc.getPages() + "</pages>");

				if (doc.getLockUserId() != null)
					writer.write("<lockUserId>" + doc.getLockUserId() + "</lockUserId>");
				if (doc.getLockUser() != null)
					writer.write("<lockUser><![CDATA[" + doc.getLockUser() + "]]></lockUser>");
				writer.write("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");
				writer.write("<icon>"
						+ FilenameUtils.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(doc.getFileName())))
						+ "</icon>");
				writer.write("<type><![CDATA[" + doc.getType() + "]]></type>");

				writer.write("<rating>" + (doc.getRating() != null ? doc.getRating() : "0") + "</rating>");
				writer.write("<fileVersion><![CDATA[" + doc.getFileVersion() + "]]></fileVersion>");
				writer.write(
						"<comment><![CDATA[" + (doc.getComment() != null ? doc.getComment() : "") + "]]></comment>");
				writer.write("<workflowStatus><![CDATA["
						+ (doc.getWorkflowStatus() != null ? doc.getWorkflowStatus() : "") + "]]></workflowStatus>");
				writer.write("<workflowStatusDisplay><![CDATA["
						+ (doc.getWorkflowStatusDisplay() != null ? doc.getWorkflowStatusDisplay() : "")
						+ "]]></workflowStatusDisplay>");
				if (doc.getStartPublishing() != null)
					writer.write("<startPublishing>" + df.format(doc.getStartPublishing()) + "</startPublishing>");
				else
					writer.write("<startPublishing></startPublishing>");
				if (doc.getStopPublishing() != null)
					writer.write("<stopPublishing>" + df.format(doc.getStopPublishing()) + "</stopPublishing>");
				else
					writer.write("<stopPublishing></stopPublishing>");
				writer.write("<publishedStatus>" + (doc.isPublishing() ? "yes" : "no") + "</publishedStatus>");

				if (doc.getExtResId() != null)
					writer.write("<extResId><![CDATA[" + doc.getExtResId() + "]]></extResId>");

				if (doc.getTemplate() != null)
					writer.write("<template><![CDATA[" + doc.getTemplate().getName() + "]]></template>");

				if (doc.getColor() != null)
					writer.write("<color><![CDATA[" + doc.getColor() + "]]></color>");

				for (String name : attrs) {
					String key = doc.getId() + "-" + name;
					Object val = extValues.get(key);
					if (val != null) {
						writer.print("<ext_" + name + ">");
						if (val instanceof Date)
							writer.print(df.format((Date) val));
						else if (val instanceof Integer)
							writer.print(Integer.toString((Integer) val));
						else if (val instanceof Long)
							writer.print(Long.toString((Long) val));
						else if (val instanceof Double)
							writer.print(Double.toString((Double) val));
						else
							writer.print("<![CDATA[" + val + "]]>");
						writer.print("</ext_" + name + ">");
					}
				}

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
				records = dao.findByObjectQuery(query.trim(), (Map<String, Object>) null, dashlet.getMax());
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
				writer.write("<icon>" + FilenameUtils
						.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(record.getFileName()))) + "</icon>");
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