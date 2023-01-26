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

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

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

		FolderDAO fldDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		StringBuilder query = new StringBuilder(
				"select ld_id, ld_name, ld_type, ld_lastmodified, ld_deleteuserid, ld_parentid, ld_deleteuser, ld_color ");
		query.append(" from ld_folder ");
		query.append(" where ld_tenantid = ");
		query.append(Long.toString(session.getTenantId()));
		query.append(" and ld_foldref is null ");
		query.append(" and ld_deleted > 0 ");

		if (deleteUserId != null) {
			query.append(" and ld_deleteuserId=");
			query.append(Long.toString(deleteUserId));
		}

		if (parentId != null) {
			query.append(" and ld_parentId=");
			query.append(Long.toString(parentId));
		}

		query.append(" order by ld_creation desc ");

		@SuppressWarnings("unchecked")
		List<Folder> records = (List<Folder>) fldDao.query(query.toString(), null, new RowMapper<Folder>() {
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
			writer.print("<id>" + fld.getId() + "</id>");
			writer.print("<name><![CDATA[" + fld.getName() + "]]></name>");
			writer.print("<lastModified>" + df.format(fld.getLastModified()) + "</lastModified>");
			writer.print("<deleteUserId>" + fld.getDeleteUserId() + "</deleteUserId>");
			writer.print("<deleteUser><![CDATA[" + (fld.getDeleteUser() != null ? fld.getDeleteUser() : "")
					+ "]]></deleteUser>");
			writer.print("<parentId>" + fld.getParentId() + "</parentId>");
			writer.print("<type>" + fld.getType() + "</type>");
			if (fld.getColor() != null)
				writer.print("<color><![CDATA[" + fld.getColor() + "]]></color>");
			writer.print("</folder>");
		}
		writer.write("</list>");
	}
}