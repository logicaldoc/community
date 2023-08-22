package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

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

		TicketDAO dao = (TicketDAO) Context.get().getBean(TicketDAO.class);
		StringBuilder query = new StringBuilder(
				"select A.ld_id, A.ld_ticketid, A.ld_docid, A.ld_creation, A.ld_expired, A.ld_count, A.ld_maxcount, A.ld_suffix, A.ld_enabled, B.ld_filename, B.ld_folderid, A.ld_views, A.ld_maxviews, A.ld_type from ld_ticket as A, ld_document as B where A.ld_deleted = 0 and (A.ld_type = "
						+ Ticket.DOWNLOAD + " or A.ld_type = " + Ticket.VIEW + ") and A.ld_tenantid="
						+ session.getTenantId()
						+ " and B.ld_deleted=0 and A.ld_docid=B.ld_id order by A.ld_creation desc");

		DateFormat df = getDateFormat();

		SqlRowSet set = dao.queryForRowSet(query.toString(), max != null ? max : 100);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next()) {
			Integer count = set.getInt(6);
			Integer maxCount = set.getInt(7);
			Integer maxViews = set.getInt(13);
			Date creation = set.getDate(4);
			Date expired = set.getDate(5);
			boolean enabled = set.getInt(9) == 1;
			String fileName = set.getString(10);
			String suffix = set.getString(8);
			if (suffix != null && StringUtils.isNotEmpty(suffix.trim()))
				fileName = fileName + ".pdf";

			writer.print("<ticket>");
			writer.print("<id>" + set.getInt(1) + "</id>");
			writer.print("<ticketId><![CDATA[" + set.getString(2) + "]]></ticketId>");
			
			writer.print("<type><![CDATA[" + set.getInt(14) + "]]></type>");
			writer.print("<docId>" + set.getLong(3) + "</docId>");
			writer.print("<creation>" + df.format(creation) + "</creation>");
			writer.print("<expired>" + df.format(expired) + "</expired>");
			
			writer.print("<count>" + count + "</count>");
			if (maxCount != null)
				writer.print("<maxCount>" + maxCount + "</maxCount>");
			
			writer.print("<views>" + set.getInt(12) + "</views>");
			if (maxViews != null)
				writer.print("<maxViews>" + maxViews + "</maxViews>");
			if (StringUtils.isNotEmpty(suffix))
				writer.print("<suffix><![CDATA[" + suffix + "]]></suffix>");
			writer.print("<eenabled>" + (enabled ? "0" : "2") + "</eenabled>");
			writer.print("<valid>" + isValidTicket(count, maxCount, expired, enabled) + "</valid>");
			writer.print("<filename><![CDATA[" + fileName + "]]></filename>");
			writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(fileName)))
					+ "</icon>");
			writer.print("<folderId>" + set.getLong(11) + "</folderId>");
			writer.print("</ticket>");
		}
		writer.write("</list>");
	}

	protected boolean isValidTicket(Integer count, Integer maxCount, Date expired, boolean enabled) {
		return enabled && (maxCount == null || maxCount <= 0 || maxCount > count)
				&& (expired == null || expired.after(new Date()));
	}
}