package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for folders data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FoldersDataServlet extends AbstractDataServlet {

	private static final String FOLDER_CLOSED = "</folder>";

	private static final String FOLDER = "<folder>";

	private static final long serialVersionUID = 1L;

	private static final String PARENT = "parent";

	public static final String FOLDER_PAGE_SIZE = "ld-folder-page-size";

	public static final String FOLDER_START_RECORD = "ld-folder-start-gridRecord";

	private static final Logger log = LoggerFactory.getLogger(FoldersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		if (request.getParameter(PARENT) != null
				&& (request.getParameter(PARENT).startsWith("d-") || request.getParameter(PARENT).equals("null"))) {
			// The user clicked on a file
			PrintWriter writer = response.getWriter();
			writer.write("<list></list>");
			return;
		}

		boolean withroot = "true".equals(request.getParameter("withroot"));
		boolean nopagination = "true".equals(request.getParameter("nopagination"));
		long tenantId = session.getTenantId();
		String tenantName = session.getTenantName();

		String parent = getParent(request, tenantId);

		long parentFolderId = getParentFolderId(parent);

		Folder parentFolder = getParentFolder(parent, parentFolderId);
		boolean root = parentFolder.getParentId() == parentFolder.getId();

		UserDAO udao = UserDAO.get();
		User user = udao.findById(session.getUserId());
		udao.initialize(user);

		/*
		 * Check if we have to paginate and what should be the start and end
		 * gridRecord numbers
		 */
		Long[] indexes = getCurrentPageExtents(session, nopagination, parentFolderId, parentFolder);
		Long startRecord = indexes[0];
		Long endRecord = indexes[1];

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if ("/".equals(request.getParameter(PARENT)) && withroot) {
			writer.print(FOLDER);
			writer.print("<id>%d</id>".formatted(parentFolder.getId()));
			writer.print("<folderId>%d</folderId>".formatted(parentFolder.getId()));
			writer.print("<parentId>%d</parentId>".formatted(parentFolder.getId()));
			writer.print("<parent>%s</parent>".formatted(parent));
			writer.print("<name><![CDATA[%s]]></name>".formatted(parentFolder.getName()));
			writer.print("<type>%d</type>".formatted(parentFolder.getType()));
			writer.print("<status>0</status>");
			writer.print("<publishedStatus>yes</publishedStatus>");
			writer.print(FOLDER_CLOSED);
		} else {
			printFolders(writer, session, tenantId, tenantName, parent, parentFolder, user, root ? null : startRecord,
					root ? null : endRecord);
		}

		if (request.getParameter("withdocs") != null) {
			printFoldersWithDocs(writer, parent, parentFolder, user);
		}

		writer.write("</list>");
	}

	private void printFolders(PrintWriter writer, Session session, long tenantId, String tenantName, String parent,
			Folder parentFolder, User user, Long startRecord, Long endRecord) throws PersistenceException {
		StringBuilder query = prepareQuery(session, tenantName, parentFolder, user);
		FolderDAO folderDao = FolderDAO.get();
		folderDao.queryForResultSet(query.toString(), Map.of("parentId", parentFolder.getId(), "tenantId", tenantId),
				null, rows -> {
					long i = 0;
					while (rows.next()) {
						if ((startRecord != null && i < startRecord) || (endRecord != null && i > endRecord)) {
							i++;
							continue;
						}

						writer.print(FOLDER);
						writer.print("<id>%s-%d</id>".formatted(parent, rows.getLong(1)));
						writer.print("<folderId>%d</folderId>".formatted(rows.getLong(1)));
						writer.print("<parentId>%d</parentId>".formatted(rows.getLong(2)));
						writer.print("<parent>%s</parent>".formatted(parent));
						writer.print("<name><![CDATA[%s]]></name>".formatted(rows.getString(3)));
						writer.print("<type>%d</type>".formatted(rows.getInt(4)));
						printFoldRef(writer, rows);
						printCustomIcon(writer, rows);
						writer.print("<status>0</status>");
						writer.print("<publishedStatus>yes</publishedStatus>");
						printColor(writer, rows);
						writer.print("<position>%d</position>".formatted(rows.getInt(7)));
						writer.print(FOLDER_CLOSED);

						i++;
					}
				});
	}

	private void printColor(PrintWriter writer, ResultSet rs) throws SQLException {
		if (StringUtils.isNotEmpty(rs.getString(6)))
			writer.print("<color><![CDATA[" + rs.getString(6) + "]]></color>");
	}

	private void printCustomIcon(PrintWriter writer, ResultSet rs) throws SQLException {
		writer.print(
				"<customIcon>" + (rs.getInt(4) == Folder.TYPE_ALIAS ? "folder_alias" : "folder") + "</customIcon>");
	}

	private void printFoldRef(PrintWriter writer, ResultSet rs) throws SQLException {
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
			FolderDAO folderDao = FolderDAO.get();
			Map<String, Object> params = new HashMap<>();
			params.put("parentId", parentFolder.getId());

			folderDao.queryForResultSet(query.toString(), params, null,
					rows -> printFoldersWithDocs(writer, rows, parent, parentFolder.getId()));
		}
	}

	private void printFoldersWithDocs(PrintWriter writer, ResultSet rs, String parent, long parentId)
			throws SQLException {
		while (rs.next()) {
			Date now = new Date();
			boolean published = (rs.getInt(4) == 1) && (rs.getDate(5) == null || now.after(rs.getDate(5)))
					&& (rs.getDate(6) == null || now.before(rs.getDate(6)));
			writer.print(FOLDER);
			writer.print("<id>d-" + rs.getLong(1) + "</id>");
			writer.print("<folderId>d-" + rs.getLong(1) + "</folderId>");
			writer.print("<parentId>" + parentId + "</parentId>");
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
			writer.print(FOLDER_CLOSED);
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
		if ("name".equals(Context.get().getConfig().getProperty(tenantName + ".gui.folder.sorting")))
			query.append(" ld_name asc ");
		else
			query.append(" ld_creation desc ");
		return query;
	}

	private void addReadConditions(StringBuilder query, Session session, Folder parentFolder)
			throws PersistenceException {
		FolderDAO folderDao = FolderDAO.get();
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

	private Long[] getCurrentPageExtents(Session session, boolean nopagination, long parentFolderId,
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
				endRecord = Context.get().getConfig().getLong(session.getTenantName() + ".gui.folder.maxchildren",
						2000L);

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

	private Folder getParentFolder(String parent, long parentFolderId) throws PersistenceException {
		FolderDAO fDao = FolderDAO.get();
		Folder parentFolder = fDao.findFolder(parentFolderId);
		if (parentFolder == null) {
			throw new PersistenceException(
					String.format("No folder found with ID=%d parent %s", parentFolderId, parent));
		} else {
			return parentFolder;
		}
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
		FolderDAO folderDao = FolderDAO.get();
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