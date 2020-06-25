package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for deleted folders data retrieval
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class DeletedFoldersDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DeletedFoldersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			final Session session = ServiceUtil.validateSession(request);

			Long parentId = request.getParameter("parentId") != null ? Long.parseLong(request.getParameter("parentId"))
					: null;

			Long deleteUserId = StringUtils.isNotEmpty(request.getParameter("userId"))
					? Long.parseLong(request.getParameter("userId"))
					: null;

			Integer max = request.getParameter("max") != null ? Integer.parseInt(request.getParameter("max")) : null;

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			FolderDAO fldDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			StringBuffer query = new StringBuffer(
					"select ld_id, ld_name, ld_type, ld_lastmodified, ld_deleteuserid, ld_parentid, ld_deleteuser ");
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

			log.error(query.toString());

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
					return folder;
				}
			}, max);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Folder fld : records) {
				writer.print("<folder>");
				writer.print("<id>" + fld.getId() + "</id>");
				writer.print("<icon>" + (fld.getType() == Folder.TYPE_ALIAS ? "folder_alias" : "folder") + "</icon>");
				writer.print("<name><![CDATA[" + fld.getName() + "]]></name>");
				writer.print("<lastModified>" + df.format(fld.getLastModified()) + "</lastModified>");
				writer.print("<deleteUserId>" + fld.getDeleteUserId() + "</deleteUserId>");
				writer.print("<deleteUser><![CDATA[" + (fld.getDeleteUser() != null ? fld.getDeleteUser() : "")
						+ "]]></deleteUser>");
				writer.print("<parentId>" + fld.getParentId() + "</parentId>");
				writer.print("<type>" + fld.getType() + "</type>");
				writer.print("</folder>");
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