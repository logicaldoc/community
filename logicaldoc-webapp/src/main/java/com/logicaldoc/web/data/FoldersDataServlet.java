package com.logicaldoc.web.data;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.core.util.ServletUtil;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for folders data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FoldersDataServlet extends HttpServlet {

	public static String FOLDER_PAGE_SIZE = "ld-folder-page-size";

	public static String FOLDER_START_RECORD = "ld-folder-start-record";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FoldersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		try {
			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			if (request.getParameter("parent") != null && (request.getParameter("parent").startsWith("d-")
					|| request.getParameter("parent").equals("null"))) {
				// The user clicked on a file
				PrintWriter writer = response.getWriter();
				writer.write("<list></list>");
				return;
			}

			boolean nopagination = "true".equals(request.getParameter("nopagination"));
			Long max = request.getParameter("max") != null ? Long.parseLong(request.getParameter("max")) : null;

			Session session = ServiceUtil.validateSession(request);
			long tenantId = session.getTenantId();
			String tenantName = session.getTenantName();

			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			String parent = "" + Folder.ROOTID;

			if (request.getParameter("parent") != null) {
				parent = request.getParameter("parent");
			} else if (request.getParameter("criteria") != null) {
				// The request comes from a menu, expecting something like
				// criteria={"fieldName":"parent","value":"5-4","operator":"equals"}
				String criteria = request.getParameter("criteria");
				parent = criteria.substring(criteria.indexOf("value") + 8);
				parent = parent.substring(0, parent.indexOf('"'));
			}

			if ("/".equals(parent)) {
				Folder root = folderDao.findRoot(tenantId);
				if (root == null)
					throw new Exception("Unable to locate the root folder for tenant " + tenantId);
				parent = "" + root.getId();
			}

			long parentFolderId = 0;
			if (parent.contains("-")) {
				parentFolderId = Long.parseLong(parent.substring(parent.lastIndexOf('-') + 1));
			} else
				parentFolderId = Long.parseLong(parent);

			Folder parentFolder = folderDao.findFolder(parentFolderId);
			if (parentFolder == null) {
				String message = String.format("No folder found with ID=%d parent %s", parentFolderId, parent);
				log.error(message);
				ServletUtil.sendError(response, message);
			}

			Context context = Context.get();
			UserDAO udao = (UserDAO) context.getBean(UserDAO.class);
			User user = udao.findById(session.getUserId());
			udao.initialize(user);

			/*
			 * Check if we have to paginate and what should be the start and end
			 * record numbers
			 */
			Long startRecord = null;
			Long endRecord = null;

			if (!nopagination && Context.get().getProperties()
					.getBoolean(session.getTenantName() + ".gui.folder.pagination", false)) {
				// Check if the folder itself specifies a max number of records
				// per page
				if (StringUtils.isNotEmpty(parentFolder.getGrid()))
					endRecord = getFolderPageSizeFromSpec(parentFolder.getGrid());

				// Go with the default page size
				if (endRecord == null)
					endRecord = Context.get().getProperties()
							.getLong(session.getTenantName() + ".gui.folder.maxchildren", 2000L);

				Integer[] pagination = new Integer[] {
						(Integer) session.getDictionary().get(FOLDER_START_RECORD + ":" + parentFolderId),
						(Integer) session.getDictionary().get(FOLDER_PAGE_SIZE + ":" + parentFolderId) };
				if (pagination != null && pagination[0] != null && pagination[1] != null) {
					log.debug("Found pagination for folder %s -> max: %s  page: %s", parentFolder, pagination[0],
							pagination[1]);
					startRecord = (long) pagination[0];
					endRecord = startRecord + pagination[1] - 1;
				}
			} else {
				endRecord = max;
			}

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			StringBuffer query = new StringBuffer(
					"select ld_id, ld_parentid, ld_name, ld_type, ld_foldref, ld_color, ld_position from ld_folder where ld_deleted=0 and ld_hidden=0 and not ld_id=ld_parentid and ld_parentid = ? and ld_tenantid = ? ");
			if (!user.isMemberOf("admin")) {
				Collection<Long> accessibleIds = folderDao.findFolderIdByUserId(session.getUserId(),
						parentFolder.getId(), false);
				if (!accessibleIds.isEmpty()) {
					List<Long> folderIds = new ArrayList<Long>(accessibleIds);
					query.append(" and ( ");

					/*
					 * Oracle has a dramatic limitation: no more than 1000
					 * elements in a list, so we have to partition the list
					 * groups of at least 1000 elements.
					 */
					int length = folderIds.size();
					int chunkSize = 1000;
					int fullChunks = (int) Math.ceil((double) length / (double) chunkSize);
					for (int chunk = 0; chunk < fullChunks; chunk++) {
						if (chunk > 0)
							query.append(" or ");

						int chunkStart = chunk * chunkSize;
						List<Long> sublist = folderIds.subList(chunkStart,
								chunkStart + chunkSize < length ? chunkStart + chunkSize : length);
						String idsStr = sublist.toString().replace('[', '(').replace(']', ')');
						query.append(" ld_id in " + idsStr);
					}

					query.append(" ) ");
				} else {
					// no folders are accessible so we do not have to return any
					// record
					query.append(" and 1 = 2");
				}
			}
			query.append(" order by ld_position asc, ");
			if ("name".equals(context.getProperties().getProperty(tenantName + ".gui.folder.sorting")))
				query.append(" ld_name asc ");
			else
				query.append(" ld_creation desc ");

			SqlRowSet rs = folderDao.queryForRowSet(query.toString(), new Long[] { parentFolder.getId(), tenantId },
					null);

			if (rs != null) {
				long i = 0;
				while (rs.next()) {
					if (startRecord != null && i < startRecord) {
						i++;
						continue;
					}
					if (endRecord != null && i > endRecord) {
						i++;
						continue;
					}

					writer.print("<folder>");
					writer.print("<id>" + parent + "-" + rs.getLong(1) + "</id>");
					writer.print("<folderId>" + rs.getLong(1) + "</folderId>");
					writer.print("<parent>" + parent + "</parent>");
					writer.print("<name><![CDATA[" + rs.getString(3) + "]]></name>");
					writer.print("<type>" + rs.getInt(4) + "</type>");
					if (rs.getObject(5) != null)
						writer.print("<foldRef>" + rs.getLong(5) + "</foldRef>");
					writer.print("<customIcon>" + (rs.getInt(4) == Folder.TYPE_ALIAS ? "folder_alias" : "folder")
							+ "</customIcon>");
					writer.print("<status>0</status>");
					writer.print("<publishedStatus>yes</publishedStatus>");
					if (StringUtils.isNotEmpty(rs.getString(6)))
						writer.print("<color><![CDATA[" + rs.getString(6) + "]]></color>");
					writer.print("<position>" + rs.getInt(7) + "</position>");
					writer.print("</folder>");

					i++;
				}
			}

			if (request.getParameter("withdocs") != null) {
				query = new StringBuffer(
						"select ld_id, ld_filename, ld_filesize, ld_published, ld_startpublishing, ld_stoppublishing, ld_status, ld_color from ld_document where ld_deleted=0 and ld_folderid=? ");
				if (!user.isMemberOf("admin") && !user.isMemberOf("publisher")) {
					query.append(" and ld_published=1");
					query.append(" and (ld_startpublishing is null or CURRENT_TIMESTAMP > ld_startpublishing) ");
					query.append(" and (ld_stoppublishing is null or CURRENT_TIMESTAMP < ld_stoppublishing) ");
				}
				query.append(" order by ld_filename");

				rs = folderDao.queryForRowSet(query.toString(), new Long[] { parentFolder.getId() }, null);
				if (rs != null)
					while (rs.next()) {
						Date now = new Date();
						boolean published = (rs.getInt(4) == 1) && (rs.getDate(5) == null || now.after(rs.getDate(5)))
								&& (rs.getDate(6) == null || now.before(rs.getDate(6)));
						writer.print("<folder>");
						writer.print("<id>d-" + rs.getLong(1) + "</id>");
						writer.print("<folderId>d-" + rs.getLong(1) + "</folderId>");
						writer.print("<parent>" + parent + "</parent>");
						writer.print("<name><![CDATA[" + rs.getString(2) + "]]></name>");
						writer.print("<type>file</type>");
						writer.print("<customIcon>"
								+ FilenameUtils
										.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(rs.getString(2))))
								+ "</customIcon>");
						writer.print("<size>" + rs.getInt(3) + "</size>");
						writer.print("<status>" + rs.getInt(7) + "</status>");
						writer.print("<publishedStatus>" + (published ? "yes" : "no") + "</publishedStatus>");
						if (StringUtils.isNotEmpty(rs.getString(8)))
							writer.print("<color><![CDATA[" + rs.getString(8) + "]]></color>");
						writer.print("<position>0</position>");
						writer.print("</folder>");
					}
			}

			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	private static Long getFolderPageSizeFromSpec(String spec) {
		if (spec != null && spec.startsWith("|")) {
			try {
				String txt = spec.substring(1, spec.indexOf('('));
				String[] tokens = txt.split("\\|");
				if (tokens.length == 2)
					return Long.parseLong(tokens[0]);
			} catch (Throwable t) {
				return null;
			}
		}
		return null;
	}
}