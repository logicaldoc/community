package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for document posts data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class NotesDataServlet extends AbstractDataServlet {

	private static final String FILE_VERSION = "fileVersion";

	protected static Logger logger = LoggerFactory.getLogger(NotesDataServlet.class);

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long userId = getUserId(request);

		Long docId = getDocId(request);

		String fileVersion = getFileVersion(request);

		Long page = getPage(request);

		StringBuilder query = new StringBuilder(
				"select A.ld_id,A.ld_message,A.ld_username,A.ld_date,A.ld_docid,B.ld_filename,A.ld_userid,A.ld_page,A.ld_color,A.ld_fileversion from ld_note A, ld_document B where A.ld_deleted=0 and B.ld_deleted=0 and A.ld_docid=B.ld_id ");
		if (userId != null)
			query.append(" and A.ld_userid =" + userId);
		if (docId != null)
			query.append(" and A.ld_docid =" + docId);
		if (page != null)
			query.append(" and A.ld_page =" + page);

		if (docId != null && fileVersion == null) {
			DocumentDAO ddao = Context.get(DocumentDAO.class);
			Document doc = ddao.findDocument(docId);
			fileVersion = doc.getFileVersion();
		}

		Map<String, Object> params = null;
		if (docId != null && StringUtils.isNotEmpty(fileVersion)) {
			query.append(" and A.ld_fileversion = :fileVersion ");
			params = Map.of(FILE_VERSION, fileVersion);
		}
		query.append(" order by A.ld_date desc, A.ld_page asc ");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DocumentNoteDAO dao = Context.get(DocumentNoteDAO.class);
		dao.queryForResultSet(query.toString(), params, 200, rows -> {
			while (rows.next())
				printPost(writer, rows);
		});

		writer.write("</list>");
	}

	private void printPost(PrintWriter writer, ResultSet set) throws SQLException {
		DateFormat df = getDateFormat();

		writer.print("<post>");
		writer.print("<id>" + set.getLong(1) + "</id>");
		writer.print("<title><![CDATA[" + StringUtils.abbreviate(set.getString(2), 100) + "]]></title>");
		if (set.getString(9) != null)
			writer.print("<noteColor><![CDATA[" + set.getString(9) + "]]></noteColor>");
		writer.print("<page>" + set.getInt(8) + "</page>");
		writer.print("<user><![CDATA[" + set.getString(3) + "]]></user>");

		Date date = null;
		Object obj = set.getObject(4);
		if (obj instanceof oracle.sql.TIMESTAMP timestamp) {
			try {
				date = new Date(timestamp.dateValue().getTime());
			} catch (SQLException e) {
				logger.warn(e.getMessage());
			}
		} else {
			date = set.getDate(4);
		}
		writer.print("<date>" + (date != null ? df.format(date) : "") + "</date>");
		writer.print("<message><![CDATA[" + set.getString(2) + "]]></message>");
		writer.print("<docId>" + set.getLong(5) + "</docId>");
		writer.print("<filename><![CDATA[" + set.getString(6) + "]]></filename>");
		writer.print("<icon>" + FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(set.getString(6))))
				+ "</icon>");
		writer.print("<userId>" + set.getString(7) + "</userId>");
		writer.print("<fileVersion><![CDATA[" + set.getString(10) + "]]></fileVersion>");
		writer.print("</post>");
	}

	private Long getPage(HttpServletRequest request) {
		Long page = null;
		if (request.getParameter("page") != null)
			page = Long.parseLong(request.getParameter("page"));
		return page;
	}

	private String getFileVersion(HttpServletRequest request) {
		String fileVersion = null;
		if (request.getParameter(FILE_VERSION) != null)
			fileVersion = request.getParameter(FILE_VERSION);
		return fileVersion;
	}

	private Long getDocId(HttpServletRequest request) throws PersistenceException {
		Long docId = null;
		if (request.getParameter("docId") != null) {
			docId = Long.parseLong(request.getParameter("docId"));
			DocumentDAO ddao = Context.get(DocumentDAO.class);
			Document doc = ddao.findDocument(docId);
			if (doc != null)
				docId = doc.getId();
		}
		return docId;
	}

	private Long getUserId(HttpServletRequest request) {
		Long userId = null;
		if (request.getParameter("userId") != null)
			userId = Long.parseLong(request.getParameter("userId"));
		return userId;
	}
}
