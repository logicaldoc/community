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

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for rendering dashlet's contents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletContent extends HttpServlet {

    private static final String LIST_TAG_CLOSED = "</list>";

    private static final String LIST_TAG = "<list>";

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(DashletContent.class);

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response) {

        try {
            Session session = validateSession(request);

            MenuDAO mDao = MenuDAO.get();
            boolean showSid = mDao.isReadAllowed(Menu.SESSIONS, session.getUserId());

            Locale locale = LocaleUtil.toLocale(request.getParameter("locale"));
            long dashletId = Long.parseLong(request.getParameter("dashletId"));

            // Avoid resource caching
            response.setHeader("Cache-Control", "no-cache,no-store,must-revalidate");
            response.setHeader("Expires", "0");
            response.setHeader("Pragma", "no-cache");
            response.setCharacterEncoding("UTF-8");

            Dashlet dashlet = DashletDAO.get().findById(dashletId);
            if (Dashlet.TYPE_CONTENT.equals(dashlet.getType()))
                response.setContentType("text/html");
            else
                response.setContentType("text/xml");

            Map<String, Object> dashletDictionary = new HashMap<>();
            dashletDictionary.put(Automation.LOCALE, locale);
            dashletDictionary.put(Automation.TENANT_ID, dashlet.getTenantId());
            dashletDictionary.put("session", session);
            dashletDictionary.put("user", session.getUser());
            dashletDictionary.put("dashlet", dashlet);

            Automation automation = new Automation("dashlet-%s".formatted(dashlet.getName()));
            PrintWriter writer = response.getWriter();

            if (Dashlet.TYPE_DOCEVENT.equals(dashlet.getType()))
                handleDocumentEvent(showSid, locale, dashlet, dashletDictionary, automation, writer);
            else if (Dashlet.TYPE_DOCUMENT.equals(dashlet.getType()))
                handleDocument(locale, dashlet, dashletDictionary, automation, writer);
            else if (Dashlet.TYPE_NOTE.equals(dashlet.getType()))
                handleNote(dashlet, dashletDictionary, automation, writer);
            else if (Dashlet.TYPE_CONTENT.equals(dashlet.getType()))
                handleContent(dashlet, dashletDictionary, automation, writer);
        } catch (NumberFormatException | ServletException | PersistenceException | IOException
                | AutomationException e) {
            log.error(e.getMessage(), e);
            try {
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
            } catch (IOException ioe) {
                // Nothing to do
            }
        }
    }

    private DateFormat getDateFormat() {
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        df.setTimeZone(TimeZone.getTimeZone("UTC"));
        return df;
    }

    private void handleDocumentEvent(
            boolean showSid,
            Locale locale,
            Dashlet dashlet,
            Map<String, Object> dashletDictionary,
            Automation automation,
            PrintWriter writer) throws PersistenceException, AutomationException {

        if (StringUtils.isNotEmpty(dashlet.getContent())) {
            String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
            if (StringUtils.isNotEmpty(content))
                writer.write(content.trim());
        } else {
            writer.write(LIST_TAG);

            String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);
            List<DocumentHistory> records = DocumentHistoryDAO.get().findByObjectQuery(query.trim(),
                    (Map<String, Object>) null, !dashlet.isUnique() ? dashlet.getMax() : null);

            List<DocumentHistory> uniqueRecords = filterUniqueDocumentEvents(dashlet, records);

            /*
             * Retrieve documents the histories refer to
             */
            Map<Long, Document> docsMap = new HashMap<>();
            if (!uniqueRecords.isEmpty()) {
                DocumentDAO ddao = DocumentDAO.get();
                List<Document> docs = ddao.findByObjectQuery(
                        "from Document where id in (%s)".formatted(uniqueRecords.stream()
                                .map(h -> Long.toString(h.getDocId())).collect(Collectors.joining(","))),
                        (Map<String, Object>) null, null);
                for (Document document : docs)
                    docsMap.put(document.getId(), document);
            }

            /*
             * Iterate over records composing the response XML document
             */
            for (DocumentHistory history : uniqueRecords)
                printDocumentEvent(showSid, locale, dashlet, uniqueRecords, docsMap, history, writer);

            writer.write(LIST_TAG_CLOSED);
        }
    }

    private void printDocumentEvent(
            boolean showSid,
            Locale locale,
            Dashlet dashlet,
            List<DocumentHistory> uniqueRecords,
            Map<Long, Document> docsMap,
            DocumentHistory history,
            PrintWriter writer) throws PersistenceException {
        DateFormat df2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
        df2.setTimeZone(TimeZone.getTimeZone("UTC"));
        DateFormat df = getDateFormat();

        writer.write("<document>");
        printField("id", history.getId(), writer);
        writer.write(String.format("<user><![CDATA[%s]]></user>", history.getUsername()));
        writer.write(String.format("<event><![CDATA[%s]]></event>", I18N.message(history.getEvent(), locale)));
        writer.write(String.format("<version>%s</version>", history.getVersion()));
        writer.write(String.format("<date>%s</date>", df2.format(history.getDate())));
        printField("comment", history.getComment(), writer);
        printField("filename", history.getFilename(), writer);
        printIcon(writer, history.getFilename());
        writer.write(String.format("<new>%b</new>", history.isNew()));
        writer.write(String.format("<folderId>%d</folderId>", history.getFolderId()));
        writer.write(String.format("<docId>%d</docId>", history.getDocId()));
        printField("path", history.getPath(), writer);
        if (showSid)
            printField("sid", history.getSessionId(), writer);
        writer.write(String.format("<userid>%d</userid>", history.getUserId()));
        printField("reason", history.getReason(), writer);
        printField("color", history.getColor(), writer);

        Document doc = docsMap.get(history.getDocId());
        if (doc != null) {
            printField("customId", doc.getCustomId(), writer);
            printField("docref", doc.getDocRef(), writer);
            printField("docrefType", doc.getDocRefType(), writer);

            printField("lastModified", history.getLastModified(), df, writer);
            printField("published", history.getDate(), df, writer);
            writer.write(String.format("<publisher><![CDATA[%s]]></publisher>", doc.getPublisher()));
            printField("created", doc.getCreation(), df, writer);

            writer.write(String.format("<creator><![CDATA[%s]]></creator>", doc.getCreator()));
            writer.write(String.format("<size>%d</size>", doc.getFileSize()));

            writer.write(String.format("<status>%s</status>", doc.getStatus()));
            writer.write(String.format("<immutable>%b</immutable>", doc.isImmutable()));
            writer.write(String.format("<indexed>%s</indexed>", doc.getIndexed()));
            writer.write(String.format("<password>%b</password>", StringUtils.isNotEmpty(doc.getPassword())));
            writer.write(String.format("<signed>%b</signed>", doc.isSigned()));
            writer.write(String.format("<stamped>%b</stamped>", doc.isStamped()));

            writer.write(String.format("<pages>%d</pages>", doc.getPages()));
            printField("lockUserId", doc.getLockUserId(), writer);
            printField("lockUser", doc.getLockUser(), writer);
            writer.write(String.format("<type><![CDATA[%s]]></type>", doc.getType()));

            writer.write(String.format("<fileVersion><![CDATA[%s]]></fileVersion>", doc.getFileVersion()));
            writer.write(String.format("<rating>%d</rating>", doc.getRating() != null ? doc.getRating() : 0));
            printField("workflowStatus", doc.getWorkflowStatus(), writer);
            printField("workflowStatusDisplay", doc.getWorkflowStatusDisplay(), writer);

            if (doc.getStartPublishing() != null)
                writer.write(
                        String.format("<startPublishing>%s</startPublishing>", df.format(doc.getStartPublishing())));
            else
                writer.write("<startPublishing></startPublishing>");
            if (doc.getStopPublishing() != null)
                writer.write(String.format("<stopPublishing>%s</stopPublishing>", df.format(doc.getStopPublishing())));
            else
                writer.write("<stopPublishing></stopPublishing>");
            writer.write(String.format("<publishedStatus>%s</publishedStatus>", doc.isPublishing() ? "yes" : "no"));

            printField("extResId", doc.getExtResId(), writer);

            if (doc.getTemplate() != null)
                writer.write(String.format("<template><![CDATA[%s]]></template>", doc.getTemplate().getName()));

            /*
             * List of names of those extended attributes declared as columns in
             * the dashlet
             */
            List<String> attrs = getExtendedAttrsNamesInDasheltColumns(dashlet);

            /*
             * Contains the extended attributes of the documents. The key is
             * documentId-atttributeName, the value is the attribute value. This
             * fieldsMap is used to maximize the listing performances.
             */
            Map<String, Object> extValues = new HashMap<>();
            retrieveExtendedAttributes(locale, uniqueRecords, extValues, attrs);

            printExtendedAttributes(df, writer, doc, attrs, extValues);
        }

        writer.write("</document>");
    }

    /**
     * Fills a map of name-value of all the extended attributes related to the
     * given history records
     * 
     * @param locale the current locale
     * @param records the list of retrieved histories
     * @param extValues The key is documentId-atttributeName, the value is the
     *        attribute value
     * @param attrs List of names of those extended attributes declared as
     *        columns in
     * @throws PersistenceException
     */
    private void retrieveExtendedAttributes(
            Locale locale,
            List<DocumentHistory> records,
            Map<String, Object> extValues,
            List<String> attrs) throws PersistenceException {

        if (attrs.isEmpty())
            return;

        log.debug("Search for extended attributes {}", attrs);

        StringBuilder qry = new StringBuilder(
                """
                 select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues
                   from ld_document_ext
                  where ld_docid in (
                """);
        qry.append(records.stream().map(d -> Long.toString(d.getDocId())).collect(Collectors.joining(",")));
        qry.append(") and ld_name in (");
        qry.append(attrs.stream().map("'%s'"::formatted).collect(Collectors.joining(",")));
        qry.append(")");

        DocumentDAO dao = DocumentDAO.get();
        dao.query(qry.toString(), new EntendedAttributesRowMapper(locale, extValues), null);
    }

    private List<DocumentHistory> filterUniqueDocumentEvents(Dashlet dashlet, List<DocumentHistory> records) {
        List<DocumentHistory> uniqueRecords = new ArrayList<>();
        if (dashlet.isUnique()) {
            log.debug("Ensure records uniqueness");

            /*
             * Make sure to have just one entry per document
             */
            Set<Long> docIds = new HashSet<>();
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
        return uniqueRecords;
    }

    private void printExtendedAttributes(
            DateFormat df,
            PrintWriter writer,
            Document doc,
            List<String> attrs,
            final Map<String, Object> extValues) {
        for (String name : attrs) {
            String key = "%d-%s".formatted(doc.getId(), name);
            Object val = extValues.get(key);
            if (val != null) {
                writer.print("<ext_%s>".formatted(name));
                switch (val) {
                    case Date date -> writer.print(df.format(date));
                    case Integer integer -> writer.print(Integer.toString(integer));
                    case Long longVal -> writer.print(Long.toString(longVal));
                    case Double doubleVal -> writer.print(Double.toString(doubleVal));
                    case String str -> writer.print(str);
                    default -> throw new IllegalArgumentException("Unexpected value: %s".formatted(val));
                }
                writer.print("</ext_%s>".formatted(name));
            }
        }
    }

    private void printField(String fieldName, Date value, DateFormat df, PrintWriter writer) {
        writer.write(String.format("<%s>%s</%s>", fieldName, value != null ? df.format(value) : "", fieldName));
    }

    private void printField(String fieldName, String value, PrintWriter writer) {
        if (StringUtils.isNotEmpty(value))
            writer.write(String.format("<%s>%s</%s>", fieldName, value, fieldName));
    }

    private void printField(String fieldName, Object value, PrintWriter writer) {
        if (value == null)
            return;
        writer.write("<%s>".formatted(fieldName));
        if (value instanceof String string)
            writer.write("<![CDATA[%s]]>".formatted(string));
        else
            writer.write(value.toString());
        writer.write("</%s>".formatted(fieldName));
    }

    private void handleDocument(
            Locale locale,
            Dashlet dashlet,
            Map<String, Object> dashletDictionary,
            Automation automation,
            PrintWriter writer) throws PersistenceException, AutomationException {

        if (StringUtils.isNotEmpty(dashlet.getContent())) {
            String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
            if (StringUtils.isNotEmpty(content))
                writer.write(content.trim());
        } else {
            DocumentDAO dao = DocumentDAO.get();
            String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);

            List<Document> records = dao.findByObjectQuery(query.trim(), (Map<String, Object>) null, dashlet.getMax());

            List<Document> uniqueRecords = filterUniqueDocuments(dashlet, records);

            List<String> attrs = getExtendedAttrsNamesInDasheltColumns(dashlet);

            /*
             * Contains the extended attributes of the documents. The key is
             * documentId-atttributeName, the value is the attribute value. This
             * fieldsMap is used to maximize the listing performances.
             */
            final Map<String, Object> extValues = new HashMap<>();

            if (!attrs.isEmpty() && !uniqueRecords.isEmpty()) {
                log.debug("Search for extended attributes {}", attrs);

                StringBuilder qry = new StringBuilder(
                        """
                        select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues
                          from ld_document_ext
                         where ld_docid in (
                        """);
                qry.append(uniqueRecords.stream().map(d -> Long.toString(d.getId())).collect(Collectors.joining(",")));
                qry.append(") and ld_name in (");
                qry.append(attrs.stream().map("'%s'"::formatted).collect(Collectors.joining(",")));
                qry.append(")");

                dao.query(qry.toString(), new EntendedAttributesRowMapper(locale, extValues), null);
            }

            writer.write(LIST_TAG);

            /*
             * Iterate over records composing the response XML document
             */
            for (Document doc : uniqueRecords) {
                printDocument(doc, attrs, extValues, writer);
            }

            writer.write(LIST_TAG_CLOSED);
        }
    }

    private void printDocument(
            Document doc,
            List<String> attrs,
            final Map<String, Object> extValues,
            PrintWriter writer) {
        DateFormat df = getDateFormat();

        writer.write("<document>");
        printField("id", doc.getId(), writer);
        printField("customId", doc.getCustomId(), writer);
        printField("docref", doc.getDocRef(), writer);
        printField("docrefType", doc.getDocRefType(), writer);

        writer.write(String.format("<version>%s</version>", doc.getVersion()));

        printField("lastModified", doc.getLastModified(), writer);
        printField("published", doc.getDate(), writer);
        writer.write(String.format("<publisher><![CDATA[%s]]></publisher>", doc.getPublisher()));

        printField("created", doc.getCreation(), writer);
        writer.write(String.format("<creator><![CDATA[%s]]></creator>", doc.getCreator()));
        writer.write(String.format("<size>%d</size>", doc.getFileSize()));

        writer.write(String.format("<status>%s</status>", doc.getStatus()));
        writer.write(String.format("<immutable>%b</immutable>", doc.isImmutable()));
        writer.write(String.format("<indexed>%s</indexed>", doc.getIndexed()));
        writer.write(String.format("<password>%b</password>", StringUtils.isNotEmpty(doc.getPassword())));
        writer.write(String.format("<signed>%b</signed>", doc.isSigned()));
        writer.write(String.format("<stamped>%b</stamped>", doc.isStamped()));

        writer.write(String.format("<pages>%d</pages>", doc.getPages()));

        printField("lockUserId", doc.getLockUserId(), writer);
        printField("lockUser", doc.getLockUser(), writer);

        writer.write(String.format("<filename><![CDATA[%s]]></filename>", doc.getFileName()));
        printIcon(writer, doc.getFileName());
        writer.write(String.format("<type><![CDATA[%s]]></type>", doc.getType()));

        writer.write(String.format("<rating>%d</rating>", doc.getRating() != null ? doc.getRating() : 0));
        writer.write(String.format("<fileVersion><![CDATA[%s]]></fileVersion>", doc.getFileVersion()));

        printField("comment", doc.getComment(), writer);
        printField("workflowStatus", doc.getWorkflowStatus(), writer);
        printField("workflowStatusDisplay", doc.getWorkflowStatusDisplay(), writer);
        printField("startPublishing", doc.getStartPublishing(), writer);
        printField("stopPublishing", doc.getStopPublishing(), writer);

        writer.write(String.format("<publishedStatus>%s</publishedStatus>", doc.isPublishing() ? "yes" : "no"));

        printField("extResId", doc.getExtResId(), writer);

        if (doc.getTemplate() != null)
            writer.write(String.format("<template><![CDATA[%s]]></template>", doc.getTemplate().getName()));

        printField("color", doc.getColor(), writer);

        printExtendedAttributes(df, writer, doc, attrs, extValues);

        writer.write("</document>");
    }

    private List<Document> filterUniqueDocuments(Dashlet dashlet, List<Document> records) {
        List<Document> uniqueRecords = new ArrayList<>();
        if (dashlet.isUnique()) {
            log.debug("Ensure records uniqueness");

            /*
             * Make sure to have just one entry per document
             */
            Set<Long> docIds = new HashSet<>();
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
        return uniqueRecords;
    }

    /**
     * Retrieves just the names of those extended attribues declared in a
     * dashlet columns
     * 
     * @param dashlet The dashlet
     * 
     * @return list of extended attribute names
     */
    private List<String> getExtendedAttrsNamesInDasheltColumns(Dashlet dashlet) {
        List<String> attrs = new ArrayList<>();
        if (StringUtils.isNotEmpty(dashlet.getColumns())) {
            StringTokenizer st = new StringTokenizer(dashlet.getColumns().trim(), ",;");
            while (st.hasMoreElements()) {
                String token = st.nextToken().trim();
                if (token.startsWith("ext_"))
                    attrs.add(token.substring(4));
            }
        }
        return attrs;
    }

    private void handleNote(
            Dashlet dashlet,
            Map<String, Object> dashletDictionary,
            Automation automation,
            PrintWriter writer) throws AutomationException {

        if (StringUtils.isNotEmpty(dashlet.getContent())) {
            String content = automation.evaluate(dashlet.getContent(), dashletDictionary);
            if (StringUtils.isNotEmpty(content))
                writer.write(content.trim());
        } else {
            writer.write(LIST_TAG);

            String query = automation.evaluate(dashlet.getQuery(), dashletDictionary);
            List<DocumentNote> records = new ArrayList<>();
            try {
                records = DocumentNoteDAO.get().findByObjectQuery(query.trim(), (Map<String, Object>) null,
                        dashlet.getMax());
            } catch (PersistenceException e) {
                log.error(e.getMessage(), e);
            }

            /*
             * Iterate over records composing the response XML document
             */
            DateFormat df = getDateFormat();
            for (DocumentNote note : records) {
                writer.write("<post>");
                printField("id", note.getId(), writer);
                writer.write(String.format(
                        "<title><![CDATA[%s]]></title>".formatted(StringUtils.abbreviate(note.getMessage(), 100))));
                writer.write(String.format("<page>%d</page>", note.getPage()));
                writer.write(String.format("<user><![CDATA[%s]]></user>", note.getUsername()));
                writer.write(String.format("<date>%s</date>", note.getDate() != null ? df.format(note.getDate()) : ""));
                writer.write(String.format("<message><![CDATA[%s]]></message>", note.getMessage()));
                writer.write(String.format("<docId>%d</docId>", note.getDocId()));
                writer.write(String.format("<filename><![CDATA[%s]]></filename>", note.getFileName()));
                printIcon(writer, note.getFileName());
                writer.write(String.format("<userId>%d</userId>", note.getUserId()));
                writer.write("</post>");
            }

            writer.write(LIST_TAG_CLOSED);
        }
    }

    private void printIcon(PrintWriter writer, String filename) {
        writer.write(String.format("<icon>%s</icon>",
                FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(filename)))));
    }

    private void handleContent(
            Dashlet dashlet,
            Map<String, Object> dashletDictionary,
            Automation automation,
            PrintWriter writer) throws AutomationException {
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

    /**
     * A row mapper we use to populate a map of extended attributes from a query
     * *
     * 
     * @author Marco Meschieri - LogicalDOC
     * @since 8.8.3
     */
    private final class EntendedAttributesRowMapper implements RowMapper<Long> {
        private final Locale locale;

        private final Map<String, Object> extValues;

        private EntendedAttributesRowMapper(Locale locale, Map<String, Object> extValues) {
            this.locale = locale;
            this.extValues = extValues;
        }

        @Override
        public Long mapRow(ResultSet rs, int row) throws SQLException {
            Long docId = rs.getLong(1);
            String name = rs.getString(2);
            int type = rs.getInt(3);

            String key = "%d-%s".formatted(docId, name);

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
            } else if (type == Attribute.TYPE_USER || type == Attribute.TYPE_FOLDER
                    || type == Attribute.TYPE_DOCUMENT) {
                extValues.put(key, rs.getString(4));
            } else if (type == Attribute.TYPE_BOOLEAN) {
                extValues.put(key, rs.getLong(5) == 1L ? I18N.message("true", locale) : I18N.message("false", locale));
            }

            return null;
        }
    }
}