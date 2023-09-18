package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for folders data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FoldersDataServlet extends AbstractDataServlet {

	private static final String PARENT = "parent";

	public static final String FOLDER_PAGE_SIZE = "ld-folder-page-size";

	public static final String FOLDER_START_RECORD = "ld-folder-start-gridRecord";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long maxChildren = getMaxChildren(session, max);

		if (request.getParameter(PARENT) != null
				&& (request.getParameter(PARENT).startsWith("d-") || request.getParameter(PARENT).equals("null"))) {
			// The user clicked on a file
			PrintWriter writer = response.getWriter();
			writer.write("<list></list>");
			return;
		}

		boolean nopagination = "true".equals(request.getParameter("nopagination"));
		long tenantId = session.getTenantId();
		String tenantName = session.getTenantName();

		String parent = getParent(request, tenantId);

		long parentFolderId = getParentFolderId(parent);

		Folder parentFolder = getParentFolder(response, parent, parentFolderId);

		Context context = Context.get();
		UserDAO udao = (UserDAO) context.getBean(UserDAO.class);
		User user = udao.findById(session.getUserId());
		udao.initialize(user);

		/*
		 * Check if we have to paginate and what should be the start and end
		 * gridRecord numbers
		 */
		Long[] indexes = getCurrentPageExtents(session, maxChildren, nopagination, parentFolderId, parentFolder);
		Long startRecord = indexes[0];
		Long endRecord = indexes[1];

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (parentFolder != null) {
			printFolders(writer, session, tenantId, tenantName, parent, parentFolder, user, startRecord, endRecord);
		}

		if (request.getParameter("withdocs") != null) {
			printFoldersWithDocs(writer, parent, parentFolder, user);
		}

		writer.write("</list>");
	}

	private void printFolders(PrintWriter writer, Session session, long tenantId, String tenantName, String parent,
			Folder parentFolder, User user, Long startRecord, Long endRecord) throws PersistenceException {
		StringBuilder query = prepareQuery(session, tenantName, parentFolder, user);
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Map<String, Object> params = new HashMap<>();
		params.put("parentId", parentFolder.getId());
		params.put("tenantId", tenantId);
		SqlRowSet rs = folderDao.queryForRowSet(query.toString(), params, null);

		if (rs != null) {
			long i = 0;
			while (rs.next()) {
				if ((startRecord != null && i < startRecord) || (endRecord != null && i > endRecord)) {
					i++;
					continue;
				}

				writer.print("<folder>");
				writer.print("<id>" + parent + "-" + rs.getLong(1) + "</id>");
				writer.print("<folderId>" + rs.getLong(1) + "</folderId>");
				writer.print("<parent>" + parent + "</parent>");
				writer.print("<name><![CDATA[" + rs.getString(3) + "]]></name>");
				writer.print("<type>" + rs.getInt(4) + "</type>");
				printFoldRef(writer, rs);
				printCustomIcon(writer, rs);
				writer.print("<status>0</status>");
				writer.print("<publishedStatus>yes</publishedStatus>");
				printColor(writer, rs);
				writer.print("<position>" + rs.getInt(7) + "</position>");
				writer.print("</folder>");

				i++;
			}
		}
	}

	private void printColor(PrintWriter writer, SqlRowSet rs) {
		if (StringUtils.isNotEmpty(rs.getString(6)))
			writer.print("<color><![CDATA[" + rs.getString(6) + "]]></color>");
	}

	private void printCustomIcon(PrintWriter writer, SqlRowSet rs) {
		writer.print(
				"<customIcon>" + (rs.getInt(4) == Folder.TYPE_ALIAS ? "folder_alias" : "folder") + "</customIcon>");
	}

	private void printFoldRef(PrintWriter writer, SqlRowSet rs) {
		if (rs.getObject(5) != null)
			writer.print("<foldRef>" + rs.getLong(5) + "</foldRef>");
	}

	private void printFoldersWithDocs(PrintWriter writer, String parent, Folder parentFolder, User user)
			throws PersistenceException {
		StringBuilder query = new StringBuilder(
				"select ld_id, ld_filename, ld_filesize, ld_published, ld_startpublishing, ld_stoppublishing, ld_status, ld_color from ld_document where ld_deleted=0 and ld_folderid=:parentId ");
		if (!user.isMemberOf(Group.GROUP_ADMIN) && !user.isMemberOf("publisher")) {
			query.append(" and ld_published=1");
			query.append(" and (ld_startpublishing is null or CURRENT_TIMESTAMP > ld_startpublishing) ");
			query.append(" and (ld_stoppublishing is null or CURRENT_TIMESTAMP < ld_stoppublishing) ");
		}
		query.append(" order by ld_filename");

		if (parentFolder != null) {
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			Map<String, Object> params = new HashMap<>();
			params.put("parentId", parentFolder.getId());
			SqlRowSet rs = folderDao.queryForRowSet(query.toString(), params, null);
			if (rs != null)
				printFoldersWithDocs(writer, rs, parent);
		}
	}

	private void printFoldersWithDocs(PrintWriter writer, SqlRowSet rs, String parent) {
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
					+ FileUtil.getBaseName(IconSelector.selectIcon(FileUtil.getExtension(rs.getString(2))))
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

	private StringBuilder prepareQuery(Session session, String tenantName, Folder parentFolder, User user)
			throws PersistenceException {
		StringBuilder query = new StringBuilder(
				"select ld_id, ld_parentid, ld_name, ld_type, ld_foldref, ld_color, ld_position from ld_folder where ld_deleted=0 and ld_hidden=0 and not ld_id=ld_parentid and ld_parentid = :parentId and ld_tenantid = :tenantId ");
		if (!user.isMemberOf(Group.GROUP_ADMIN) && parentFolder != null) {
			addReadConditions(query, session, parentFolder);
		}
		query.append(" order by ld_position asc, ");
		if ("name".equals(Context.get().getProperties().getProperty(tenantName + ".gui.folder.sorting")))
			query.append(" ld_name asc ");
		else
			query.append(" ld_creation desc ");
		return query;
	}

	private void addReadConditions(StringBuilder query, Session session, Folder parentFolder)
			throws PersistenceException {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Collection<Long> accessibleIds = folderDao.findFolderIdByUserId(session.getUserId(), parentFolder.getId(),
				false);
		if (!accessibleIds.isEmpty()) {
			List<Long> folderIds = new ArrayList<>(accessibleIds);
			query.append(" and ( ");

			/*
			 * Oracle has a dramatic limitation: no more than 1000 elements in a
			 * list, so we have to partition the list groups of at least 1000
			 * elements.
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
			// gridRecord
			query.append(" and 1 = 2");
		}
	}

	private Long[] getCurrentPageExtents(Session session, Long maxChildren, boolean nopagination, long parentFolderId,
			Folder parentFolder) {
		Long startRecord = null;
		Long endRecord = null;

		if (!nopagination) {
			// Check if the folder itself specifies a max number of records
			// per page
			if (parentFolder != null && StringUtils.isNotEmpty(parentFolder.getGrid()))
				endRecord = getFolderPageSizeFromSpec(parentFolder.getGrid());
	
			// Go with the default page size
			if (endRecord == null)
				endRecord = Long.MAX_VALUE;

			Integer[] pagination = new Integer[] {
					(Integer) session.getDictionary().get(FOLDER_START_RECORD + ":" + parentFolderId),
					(Integer) session.getDictionary().get(FOLDER_PAGE_SIZE + ":" + parentFolderId) };
			if (pagination[0] != null && pagination[1] != null) {
				log.debug("Found pagination for folder {} -> max: {}  page: {}", parentFolder, pagination[0],
						pagination[1]);
				startRecord = (long) pagination[0];
				endRecord = startRecord + pagination[1] - 1;
			}
		} else {
			endRecord = Long.MAX_VALUE;
		}

		return new Long[] { startRecord, endRecord };
	}

	private Folder getParentFolder(HttpServletResponse response, String parent, long parentFolderId)
			throws PersistenceException {
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder parentFolder = fDao.findFolder(parentFolderId);
		if (parentFolder == null) {
			String message = String.format("No folder found with ID=%d parent %s", parentFolderId, parent);
			log.error(message);
			try {
				response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
			} catch (IOException e) {
				// Nothing to do
			}
		}
		return parentFolder;
	}

	private long getParentFolderId(String parent) {
		long parentFolderId = 0;
		if (parent.contains("-")) {
			parentFolderId = Long.parseLong(parent.substring(parent.lastIndexOf('-') + 1));
		} else
			parentFolderId = Long.parseLong(parent);
		return parentFolderId;
	}

	private String getParent(HttpServletRequest request, long tenantId) throws PersistenceException, IOException {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		String parent = "" + Folder.ROOTID;

		if (request.getParameter(PARENT) != null) {
			parent = request.getParameter(PARENT);
		} else if (request.getParameter("criteria") != null) {
			String criteria = request.getParameter("criteria");
			parent = criteria.substring(criteria.indexOf("value") + 8);
			parent = parent.substring(0, parent.indexOf('"'));
		}
		if ("/".equals(parent)) {
			Folder root = folderDao.findRoot(tenantId);
			if (root == null)
				throw new IOException("Unable to locate the root folder for tenant " + tenantId);
			parent = "" + root.getId();
		}
		return parent;
	}

	private long getMaxChildren(Session session, Integer max) {
		long maxChildren = Context.get().getProperties().getLong(session.getTenantName() + ".gui.folder.maxchildren",
				2000L);
		if (max != null)
			maxChildren = max;
		return maxChildren;
	}

	private static Long getFolderPageSizeFromSpec(String spec) {
		if (spec != null && spec.startsWith("|")) {
			try {
				String txt = spec.substring(1, spec.indexOf('('));
				String[] tokens = txt.split("\\|");
				if (tokens.length == 2)
					return Long.parseLong(tokens[0]);
			} catch (Exception t) {
				return null;
			}
		}
		return null;
	}
}