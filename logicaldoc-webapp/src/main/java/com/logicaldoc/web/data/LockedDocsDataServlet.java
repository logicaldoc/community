package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for locked documents data retrieval
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class LockedDocsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long userId = request.getParameter("userId") != null ? Long.parseLong(request.getParameter("userId")) : null;

		DocumentDAO docDao = DocumentDAO.get();
		DateFormat df = getDateFormat();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder("""
                                                select A.ld_id, A.ld_customid, A.ld_type, A.ld_version, A.ld_lastmodified, 
                                                       A.ld_publisher, A.ld_filesize, A.ld_filename, A.ld_immutable, A.ld_folderid, A.ld_status, A.ld_lockuserid
                                                       B.ld_firstname, B.ld_name, A.ld_fileversion, A.ld_color, A.ld_fileversion
                                                  from ld_document A 
                                                  left outer join ld_user B on A.ld_lockuserid = B.ld_id
                                                 where A.ld_deleted = 0 
                                                   and not A.ld_status = %d
                                                   and A.ld_tenantid = %d
                                                   and A.ld_docref is null
                                                   and (not A.ld_status = 0 or not A.ld_immutable = 0)
                                                """.formatted(DocumentStatus.ARCHIVED.ordinal(),
				session.getTenantId()));
		if (userId != null) {
			query.append(" and A.ld_lockuserid=");
			query.append(Long.toString(userId));
		}

		List<Document> records = docDao.query(query.toString(), new RowMapper<Document>() {
			public Document mapRow(ResultSet rs, int rowNum) throws SQLException {
				Document doc = new Document();
				doc.setTenantId(session.getTenantId());
				doc.setId(rs.getLong(1));
				doc.setCustomId(rs.getString(2));
				doc.setType(rs.getString(3));
				doc.setVersion(rs.getString(4));
				doc.setLastModified(new Date(rs.getTimestamp(5).getTime()));
				doc.setPublisher(rs.getString(6));
				doc.setFileSize(rs.getLong(7));
				doc.setFileName(rs.getString(8));
				doc.setImmutable(rs.getInt(9) == 1);
				Folder folder = new Folder();
				folder.setId(rs.getLong(10));
				folder.setTenantId(session.getTenantId());
				doc.setFolder(folder);

				doc.setStatus(rs.getInt(11));
				doc.setLockUserId(rs.getLong(12));

				// Use the comment to store the locker
				doc.setComment(rs.getString(13) + " " + rs.getString(14));

				doc.setFileVersion(rs.getString(15));
				doc.setColor(rs.getString(16));
				doc.setFileVersion(rs.getString(17));

				return doc;
			}
		}, max);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Document doc : records) {
			writer.print("<document>");
			writer.print(String.format("<id>%d</id>", doc.getId()));
			writer.print(
					String.format("<customId><![CDATA[%s]]></customId>", StringUtils.defaultString(doc.getCustomId())));
			writer.print(String.format("<icon>%s</icon>", doc.getIcon()));
			writer.print(String.format("<version>%s</version>", doc.getVersion()));
			writer.print(String.format("<fileVersion>%s</fileVersion>", doc.getFileVersion()));
			writer.print(String.format("<lastModified>%s</lastModified>", df.format(doc.getLastModified())));
			writer.print(String.format("<size>%d</size>", doc.getFileSize()));
			writer.print(String.format("<filename><![CDATA[%s]]></filename>", doc.getFileName()));
			writer.print(String.format("<immutable>%b</immutable>", doc.isImmutable()));
			writer.print(String.format("<folderId>%d</folderId>", doc.getFolder().getId()));
			writer.print(String.format("<type>%s</type>", doc.getType()));
			writer.print(String.format("<status>%s</status>", doc.getStatus()));
			writer.print(String.format("<userId>%d</userId>", doc.getLockUserId()));
			writer.print(String.format("<avatar>%d</avatar>", doc.getLockUserId()));
			if (doc.getComment() != null)
				writer.print(String.format("<username>%s</username>", doc.getComment()));
			if (doc.getColor() != null)
				writer.print(String.format("<color><![CDATA[%s]]></color>", doc.getColor()));
			writer.print("</document>");
		}
		writer.write("</list>");
	}
}
