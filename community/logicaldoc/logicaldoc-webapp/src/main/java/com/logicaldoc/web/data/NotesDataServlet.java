package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for document posts data.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class NotesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(NotesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			Long userId = null;
			if (request.getParameter("userId") != null)
				userId = Long.parseLong(request.getParameter("userId"));

			Long docId = null;
			if (request.getParameter("docId") != null)
				docId = Long.parseLong(request.getParameter("docId"));

			Long page = null;
			if (request.getParameter("page") != null)
				page = Long.parseLong(request.getParameter("page"));

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			StringBuffer query = new StringBuffer(
					"select A.ld_id,A.ld_message,A.ld_username,A.ld_date,A.ld_docid,B.ld_filename,A.ld_userid,A.ld_page,A.ld_snippet from ld_note A, ld_document B where A.ld_deleted=0 and B.ld_deleted=0 and A.ld_docid=B.ld_id ");
			if (userId != null)
				query.append(" and A.ld_userid =" + userId);
			if (docId != null)
				query.append(" and A.ld_docid =" + docId);
			if (page != null)
				query.append(" and A.ld_page =" + page);
			query.append(" order by A.ld_date desc, A.ld_page asc ");

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			SqlRowSet set = dao.queryForRowSet(query.toString(), null, 200);

			while (set.next()) {
				writer.print("<post>");
				writer.print("<id>" + set.getLong(1) + "</id>");
				writer.print("<title><![CDATA[" + StringUtils.abbreviate(set.getString(2), 100) + "]]></title>");
				if (set.getString(9) != null)
					writer.print("<snippet><![CDATA[" + set.getString(9) + "]]></snippet>");
				writer.print("<page>" + set.getInt(8) + "</page>");
				writer.print("<user><![CDATA[" + set.getString(3) + "]]></user>");
				writer.print("<date>" + (set.getDate(4) != null ? df.format(set.getDate(4)) : "") + "</date>");
				writer.print("<message><![CDATA[" + set.getString(2) + "]]></message>");
				writer.print("<docId>" + set.getLong(5) + "</docId>");
				writer.print("<docFilename><![CDATA[" + set.getString(6) + "]]></docFilename>");
				writer.print("<userId><![CDATA[" + set.getString(7) + "]]></userId>");
				writer.print("</post>");
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
