package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for listing the tickets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TicketsDataServlet extends AbstractDataServlet {

    private static final long serialVersionUID = 1L;

    @Override
    protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
            Locale locale) throws PersistenceException, IOException {

        PrintWriter writer = response.getWriter();
        writer.write("<list>");

        TicketDAO dao = TicketDAO.get();
        String query = """
 select A.ld_id, A.ld_ticketid, A.ld_docid, A.ld_creation, A.ld_expired, A.ld_count, A.ld_maxcount, A.ld_suffix, 
 A.ld_enabled, B.ld_filename, B.ld_folderid, A.ld_views, A.ld_maxviews, A.ld_type, A.ld_password 
 from ld_ticket as A, ld_document as B 
where A.ld_deleted = 0 
  and (A.ld_type = %d or A.ld_type = %d)
  and A.ld_tenantid = %d
  and B.ld_deleted = 0 
  and A.ld_docid = B.ld_id 
order by A.ld_creation desc
""".formatted(Ticket.DOWNLOAD, Ticket.VIEW, session.getTenantId());

        DateFormat df = getDateFormat();
        dao.queryForResultSet(query, null, max != null ? max : 100, rows -> {
            /*
             * Iterate over records composing the response XML document
             */
            while (rows.next())
                printTicket(writer, rows, df);
        });

        writer.write("</list>");
    }

    private void printTicket(PrintWriter writer, ResultSet rows, DateFormat df) throws SQLException {
        Integer count = rows.getInt(6);
        Integer maxCount = rows.getInt(7);
        Integer maxViews = rows.getInt(13);
        Date creation = rows.getDate(4);
        Date expired = rows.getDate(5);
        boolean enabled = rows.getInt(9) == 1;
        String fileName = rows.getString(10);
        String suffix = rows.getString(8);
        if (suffix != null && StringUtils.isNotEmpty(suffix.trim()))
            fileName = "%s.pdf".formatted(fileName);

        writer.print("<ticket>");
        writer.print(String.format("<id>%d</id>", rows.getLong(1)));
        writer.print(String.format("<ticketId><![CDATA[%s]]></ticketId>", rows.getString(2)));

        writer.print(String.format("<type><![CDATA[%d]]></type>", rows.getInt(14)));
        writer.print(String.format("<docId>%d</docId>", rows.getLong(3)));
        writer.print(String.format("<creation>%s</creation>", df.format(creation)));
        writer.print(String.format("<expired>%s</expired>", df.format(expired)));

        writer.print(String.format("<count>%d</count>", count));
        if (maxCount != null)
            writer.print(String.format("<maxCount>%d</maxCount>", maxCount));

        writer.print(String.format("<views>%d</views>", rows.getInt(12)));
        if (maxViews != null)
            writer.print(String.format("<maxViews>%d</maxViews>", maxViews));
        if (StringUtils.isNotEmpty(suffix))
            writer.print(String.format("<suffix><![CDATA[%s]]></suffix>", suffix));
        writer.print(String.format("<eenabled>%b</eenabled>", enabled));
        writer.print(String.format("<valid>%b</valid>", isValidTicket(count, maxCount, expired, enabled)));
        writer.print(String.format("<filename><![CDATA[%s]]></filename>", fileName));
        writer.print(String.format("<icon>%s</icon>",
                FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(fileName)))));
        writer.print(String.format("<folderId>%d</folderId>", rows.getLong(11)));
        writer.print(String.format("<password>%b</password>", rows.getObject(15) != null));
        writer.print("</ticket>");
    }

    protected boolean isValidTicket(Integer count, Integer maxCount, Date expired, boolean enabled) {
        return enabled && (maxCount == null || maxCount <= 0 || maxCount > count)
                && (expired == null || expired.after(new Date()));
    }
}