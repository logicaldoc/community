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
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for user history data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class UserHistoryDataServlet extends AbstractDataServlet {

    private static final String TENANT_ID = "tenantId";

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
            Locale locale) throws PersistenceException, IOException {

        MenuDAO mDao = MenuDAO.get();
        boolean showSid = mDao.isReadAllowed(Menu.SESSIONS, session.getUserId());

        Long userId = StringUtils.isNotEmpty(request.getParameter("id")) ? Long.parseLong(request.getParameter("id"))
                : null;
        Long tenantId = StringUtils.isNotEmpty(request.getParameter(TENANT_ID))
                ? Long.parseLong(request.getParameter(TENANT_ID))
                : null;
        String comment = request.getParameter("comment");
        String event = request.getParameter("event");

        List<?> records = executeQuery(max, tenantId, userId, event, comment);

        PrintWriter writer = response.getWriter();
        writer.write("<list>");

        DateFormat df = getDateFormat();

        /*
         * Iterate over the collection of user histories
         */
        for (Object gridRecord : records)
            printHistory(writer, (Object[]) gridRecord, locale, showSid, df);

        writer.write("</list>");
    }

    private void printHistory(PrintWriter writer, Object[] columns, Locale locale, boolean showSid, DateFormat df) {
        writer.print("<history>");
        writer.print(String.format("<id>%d</id>", SqlUtil.getColumnLongValue(columns[0])));
        writer.print(String.format("<user><![CDATA[%s]]></user>", columns[1]));
        writer.print(String.format("<event><![CDATA[%s]]></event>", I18N.message((String) columns[2], locale)));
        writer.print(String.format("<date>%s</date>", df.format(SqlUtil.getColumnDateValue(columns[3]))));
        if (columns[4] != null)
            writer.print(String.format("<comment><![CDATA[%s]]></comment>", columns[4]));
        if (columns[5] != null && showSid)
            writer.print(String.format("<sid><![CDATA[%s]]></sid>", columns[5]));
        writer.print(String.format("<userId>%d</userId>", SqlUtil.getColumnLongValue(columns[6])));
        if (columns[7] != null)
            writer.print(String.format("<ip><![CDATA[%s]]></ip>", columns[7]));
        if (columns[8] != null)
            writer.print(String.format("<device><![CDATA[%s]]></device>", columns[8]));
        if (columns[9] != null)
            writer.print(String.format("<geolocation><![CDATA[%s]]></geolocation>", columns[9]));
        if (columns[10] != null)
            writer.write(String.format("<key><![CDATA[%s]]></key>", columns[10]));
        writer.write(String.format("<impersonator>%s</impersonator>", StringUtils.defaultString((String) columns[11])));
        writer.print("</history>");
    }

    private List<?> executeQuery(Integer max, Long tenantId, Long userId, String event, String comment)
            throws PersistenceException {
        Map<String, Object> params = new HashMap<>();

        StringBuilder query = new StringBuilder("""
select A.id, A.username, A.event, A.date, A.comment, A.sessionId, A.userId, A.ip, A.device, A.geolocation, A.keyLabel, A.impersonator 
  from UserHistory A 
 where A.deleted = 0
""");
        if (StringUtils.isNotEmpty(event)) {
            query.append(" and A.event = :event ");
            params.put("event", event);
        }
        if (tenantId != null) {
            query.append(" and A.tenantId = :tenantId ");
            params.put(TENANT_ID, tenantId);
        }
        if (userId != null) {
            query.append(" and A.userId = :userId ");
            params.put("userId", userId);
        }
        if (StringUtils.isNotEmpty(comment)) {
            query.append(" and A.comment like :comment ");
            params.put("comment", "%s%%".formatted(comment));
        }

        query.append(" order by A.date desc ");

        return UserHistoryDAO.get().findByQuery(query.toString(), params, max != null ? max : 100);
    }
}