package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for deleted documents data retrieval
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.2.1
 */
public class DeletedDocsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale) throws PersistenceException, IOException {

		Long folderId = request.getParameter("folderId") != null ? Long.parseLong(request.getParameter("folderId"))
				: null;

		Long deleteUserId = StringUtils.isNotEmpty(request.getParameter("userId"))
				? Long.parseLong(request.getParameter("userId"))
				: null;

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		// Avoid resource caching
		response.setHeader("Pragma", "no-cache");
		response.setHeader("Cache-Control", "no-store");
		response.setDateHeader("Expires", 0);

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuffer query = new StringBuffer(
				"select A.ld_id, A.ld_customid, A.ld_type, A.ld_version, A.ld_fileversion, A.ld_lastmodified, ");
		query.append(
				" A.ld_publisher, A.ld_filesize, A.ld_filename, A.ld_folderid, B.ld_name, A.ld_creation, A.ld_deleteuserid, A.ld_deleteuser ");
		query.append(" from ld_document A ");
		query.append(" left outer join ld_folder B on A.ld_folderid=B.ld_id ");
		query.append(" where A.ld_tenantid=");
		query.append(Long.toString(session.getTenantId()));
		query.append(" and A.ld_docref is null ");
		query.append(" and A.ld_deleted > 0 ");

		if (deleteUserId != null) {
			query.append(" and A.ld_deleteuserId=");
			query.append(Long.toString(deleteUserId));
		}

		if (folderId != null) {
			query.append(" and A.ld_folderId=");
			query.append(Long.toString(folderId));
		}

		query.append(" order by A.ld_creation desc ");

		log.error(query.toString());

		@SuppressWarnings("unchecked")
		List<Document> records = (List<Document>) docDao.query(query.toString(), null, new RowMapper<Document>() {
			public Document mapRow(ResultSet rs, int rowNum) throws SQLException {
				Document doc = new Document();
				doc.setTenantId(session.getTenantId());
				doc.setId(rs.getLong(1));
				doc.setCustomId(rs.getString(2));
				doc.setType(rs.getString(3));
				doc.setVersion(rs.getString(4));
				doc.setFileVersion(rs.getString(5));
				doc.setLastModified(new Date(rs.getTimestamp(6).getTime()));
				doc.setPublisher(rs.getString(7));
				doc.setFileSize(rs.getLong(8));
				doc.setFileName(rs.getString(9));
				Folder folder = new Folder();
				folder.setId(rs.getLong(10));
				folder.setName(rs.getString(11));
				folder.setTenantId(session.getTenantId());
				doc.setFolder(folder);

				doc.setCreation(new Date(rs.getTimestamp(12).getTime()));
				doc.setDeleteUserId(rs.getLong(13));
				doc.setDeleteUser(rs.getString(14));

				return doc;
			}
		}, max);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Document doc : records) {
			writer.print("<document>");
			writer.print("<id>" + doc.getId() + "</id>");
			if (doc.getCustomId() != null)
				writer.print("<customId><![CDATA[" + doc.getCustomId() + "]]></customId>");
			else
				writer.print("<customId> </customId>");
			writer.print("<icon>"
					+ FilenameUtils.getBaseName(IconSelector.selectIcon(doc.getType(), doc.getDocRef() != null))
					+ "</icon>");
			writer.print("<version>" + doc.getVersion() + "</version>");
			writer.print("<fileVersion>" + doc.getFileVersion() + "</fileVersion>");
			writer.print("<lastModified>" + df.format(doc.getLastModified()) + "</lastModified>");
			writer.print("<created>" + df.format(doc.getCreation()) + "</created>");
			writer.print("<size>" + doc.getFileSize() + "</size>");
			writer.print("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");
			writer.print("<immutable>" + doc.getImmutable() + "</immutable>");
			writer.print("<deleteUserId>" + doc.getDeleteUserId() + "</deleteUserId>");
			writer.print("<avatar>" + doc.getDeleteUserId() + "</avatar>");
			writer.print("<deleteUser><![CDATA[" + (doc.getDeleteUser() != null ? doc.getDeleteUser() : "")
					+ "]]></deleteUser>");
			writer.print("<folderId>" + doc.getFolder().getId() + "</folderId>");
			writer.print("<folder><![CDATA[" + doc.getFolder().getName() + "]]></folder>");
			writer.print("<type>" + doc.getType() + "</type>");
			writer.print("</document>");
		}
		writer.write("</list>");
	}
}
