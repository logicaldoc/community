package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for deleted folders data retrieval
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class DeletedFoldersDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long parentId = request.getParameter("parentId") != null ? Long.parseLong(request.getParameter("parentId"))
				: null;

		Long deleteUserId = StringUtils.isNotEmpty(request.getParameter("userId"))
				? Long.parseLong(request.getParameter("userId"))
				: null;

		FolderDAO fldDao = FolderDAO.get();
		DateFormat df = getDateFormat();

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder("""
				                                select ld_id, ld_name, ld_type, ld_lastmodified, ld_deleteuserid, ld_parentid, ld_deleteuser, ld_color 
                                                  from ld_folder
                                                 where ld_tenantid = %d
                                                   and ld_foldref is null
                                                   and ld_deleted > 0                                                                             				                                
                                                """.formatted(session.getTenantId()));

		if (deleteUserId != null)
			query.append(" and ld_deleteuserId = %d".formatted(deleteUserId));

		if (parentId != null)
			query.append(" and ld_parentId = %d".formatted(parentId));

		query.append(" order by ld_creation desc ");

		List<Folder> records = fldDao.query(query.toString(), new RowMapper<Folder>() {
			public Folder mapRow(ResultSet rs, int rowNum) throws SQLException {
				Folder folder = new Folder();
				folder.setTenantId(session.getTenantId());
				folder.setId(rs.getLong(1));
				folder.setName(rs.getString(2));
				folder.setType(rs.getInt(3));
				folder.setLastModified(new Date(rs.getTimestamp(4).getTime()));
				folder.setDeleteUserId(rs.getLong(5));
				folder.setParentId(rs.getLong(6));
				folder.setDeleteUser(rs.getString(7));
				folder.setColor(rs.getString(8));
				return folder;
			}
		}, max != null ? max : 100);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Folder fld : records) {
			writer.print("<folder>");
			writer.print(String.format("<id>%d</id>", fld.getId()));
			writer.print(String.format("<name><![CDATA[%s]]></name>", fld.getName()));
			writer.print(String.format("<lastModified>%s</lastModified>", df.format(fld.getLastModified())));
			writer.print(String.format("<deleteUserId>%d</deleteUserId>", fld.getDeleteUserId()));
			writer.print(String.format("<deleteUser><![CDATA[%s]]></deleteUser>",
					StringUtils.defaultString(fld.getDeleteUser())));
			writer.print(String.format("<parentId>%d</parentId>", fld.getParentId()));
			writer.print(String.format("<type>%d</type>", fld.getType()));
			if (fld.getColor() != null)
				writer.print(String.format("<color><![CDATA[%s]]></color>", fld.getColor()));
			writer.print("</folder>");
		}
		
		writer.write("</list>");
	}
}