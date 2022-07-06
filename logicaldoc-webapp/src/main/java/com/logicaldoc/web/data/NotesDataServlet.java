package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for document posts data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class NotesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(NotesDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			ServiceUtil.validateSession(request);

			Long userId = null;
			if (request.getParameter("userId") != null)
				userId = Long.parseLong(request.getParameter("userId"));

			Long docId = null;
			if (request.getParameter("docId") != null) {
				docId=Long.parseLong(request.getParameter("docId"));				
				DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc=ddao.findDocument(docId);
				if(doc!=null)
					docId=doc.getId();
			}

			String fileVersion = null;
			if (request.getParameter("fileVersion") != null)
				fileVersion = request.getParameter("fileVersion");

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
					"select A.ld_id,A.ld_message,A.ld_username,A.ld_date,A.ld_docid,B.ld_filename,A.ld_userid,A.ld_page,A.ld_color,A.ld_fileversion from ld_note A, ld_document B where A.ld_deleted=0 and B.ld_deleted=0 and A.ld_docid=B.ld_id ");
			if (userId != null)
				query.append(" and A.ld_userid =" + userId);
			if (docId != null)
				query.append(" and A.ld_docid =" + docId);
			if (page != null)
				query.append(" and A.ld_page =" + page);

			if (docId != null && fileVersion == null) {
				DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document doc = ddao.findDocument(docId);
				fileVersion = doc.getFileVersion();
			}

			DocumentNoteDAO dao = (DocumentNoteDAO) Context.get().getBean(DocumentNoteDAO.class);
			SqlRowSet set = null;
			if (docId != null && StringUtils.isNotEmpty(fileVersion)) {
				query.append(" and A.ld_fileversion = ? order by A.ld_date desc, A.ld_page asc ");
				set = dao.queryForRowSet(query.toString(), new Object[] { fileVersion }, 200);
			} else {
				query.append(" order by A.ld_date desc, A.ld_page asc ");
				set = dao.queryForRowSet(query.toString(), null, 200);
			}

			while (set.next()) {
				writer.print("<post>");
				writer.print("<id>" + set.getLong(1) + "</id>");
				writer.print("<title><![CDATA[" + StringUtils.abbreviate(set.getString(2), 100) + "]]></title>");
				if (set.getString(9) != null)
					writer.print("<noteColor><![CDATA[" + set.getString(9) + "]]></noteColor>");
				writer.print("<page>" + set.getInt(8) + "</page>");
				writer.print("<user><![CDATA[" + set.getString(3) + "]]></user>");

				Date date = null;
				if (set.getObject(4) != null && set.getObject(4).getClass().getName().equals("oracle.sql.TIMESTAMP")) {
					oracle.sql.TIMESTAMP ts = (oracle.sql.TIMESTAMP) set.getObject(4);
					date = new Date(ts.dateValue().getTime());
				} else {
					date = set.getDate(4);
				}
				writer.print("<date>" + (date != null ? df.format(date) : "") + "</date>");
				writer.print("<message><![CDATA[" + set.getString(2) + "]]></message>");
				writer.print("<docId>" + set.getLong(5) + "</docId>");
				writer.print("<filename><![CDATA[" + set.getString(6) + "]]></filename>");
				writer.print("<icon>"
						+ FilenameUtils.getBaseName(IconSelector.selectIcon(FilenameUtils
								.getExtension(set.getString(6)))) + "</icon>");
				writer.print("<userId>" + set.getString(7) + "</userId>");
				writer.print("<fileVersion><![CDATA[" + set.getString(10) + "]]></fileVersion>");
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
