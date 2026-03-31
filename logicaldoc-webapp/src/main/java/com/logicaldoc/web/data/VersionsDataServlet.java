package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for document versions data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class VersionsDataServlet extends AbstractDataServlet {

    private static final String DOC_ID = "docId";

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
            Locale locale) throws PersistenceException, IOException {

        List<?> records = executeQuery(request, max);

        /*
         * Iterate over records composing the response XML document
         */
        DateFormat df = getDateFormat();

        PrintWriter writer = response.getWriter();
        writer.write("<list>");

        for (Object gridRecord : records) {
            Object[] cols = (Object[]) gridRecord;

            writer.print("<version>");
            writer.print(String.format("<id>%d</id>", SqlUtil.getColumnLongValue(cols[0])));
            writer.print(String.format("<user><![CDATA[%s]]></user>", StringUtils.defaultString((String) cols[1])));
            writer.print(String.format("<event><![CDATA[%s]]></event>", I18N.message((String) cols[2], locale)));
            writer.print(String.format("<version>%s</version>", cols[3]));
            writer.print(String.format("<fileVersion>%s</fileVersion>", cols[4]));
            writer.print(String.format("<revision>%s</revision>", cols[17]));
            writer.print(String.format("<date>%s</date>", df.format(SqlUtil.getColumnDateValue(cols[5]))));
            writer.print(
                    String.format("<comment><![CDATA[%s]]></comment>", StringUtils.defaultString((String) cols[6])));
            writer.print(String.format("<docid>%d</docid>", SqlUtil.getColumnLongValue(cols[7])));
            writer.print(String.format("<filename><![CDATA[%s]]></filename>", cols[8]));
            writer.print(
                    String.format("<customid><![CDATA[%s]]></customid>", StringUtils.defaultString((String) cols[9])));
            writer.print(String.format("<size>%d</size>", SqlUtil.getColumnLongValue(cols[10])));
            writer.print(
                    String.format("<icon>%s</icon>", FileUtil.getBaseName(IconSelector.selectIcon((String) cols[11]))));
            writer.print(String.format("<type>%s</type>", cols[11]));

            if (cols[12] != null)
                writer.print(String.format("<template><![CDATA[%s]]></template>", cols[12]));
            if (cols[13] != null)
                writer.print(String.format("<workflowStatus><![CDATA[%s]]></workflowStatus>", cols[13]));
            if (cols[14] != null)
                writer.print(String.format("<workflowStatusDisplay><![CDATA[%s]]></workflowStatusDisplay>", cols[14]));
            writer.print(String.format("<userId>%d</userId>", SqlUtil.getColumnLongValue(cols[15])));
            if (cols[16] != null)
                writer.print(String.format("<color><![CDATA[%s]]></color>", cols[16]));

            writer.print("</version>");
        }

        writer.write("</list>");
    }

    private List<?> executeQuery(HttpServletRequest request, Integer max) throws PersistenceException {
        VersionDAO dao = VersionDAO.get();

        Map<String, Object> params = new HashMap<>();

        StringBuilder query = new StringBuilder("""
select A.id, A.username, A.event, A.version, A.fileVersion, A.versionDate, A.comment, A.docId, A.fileName, A.customId, 
       A.fileSize, A.type, A.templateName, A.workflowStatus, A.workflowStatusDisplay, A.userId, A.color, A.revision 
""");
        if (request.getParameter(DOC_ID) != null) {
            long docId = Long.parseLong(request.getParameter(DOC_ID));
            DocumentDAO ddao = DocumentDAO.get();
            Document doc = ddao.findDocument(docId);
            if (doc != null)
                docId = doc.getId();

            query.append(" from Version A where A.deleted = 0 and A.docId = :docId ");
            params.put(DOC_ID, docId);
        } else {
            query.append(
                    " from Version A, Archive B where A.deleted = 0 and A in elements(B.entries) and B.id = :archiveId ");
            params.put("archiveId", Long.parseLong(request.getParameter("archiveId")));
        }
        query.append(" order by A.versionDate desc ");

        return dao.findByQuery(query.toString(), params, max != null ? max : 100);
    }
}