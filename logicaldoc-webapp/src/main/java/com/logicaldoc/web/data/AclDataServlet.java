package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.DocumentNoteDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.GroupType;
import com.logicaldoc.core.security.user.UserDAO;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible retrieving the Access Control List for documents,
 * folders, menus and templates
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class AclDataServlet extends AbstractDataServlet {

	private static final String AVATAR_GROUP_AVATAR = "<avatar>group</avatar>";

	private static final String TYPE_CLOSED = "</type>";

	private static final String TYPE = "<type>";

	private static final String READ_CLOSED = "</read>";

	private static final String READ = "<read>";

	private static final String WRITE = "<write>";

	private static final String WRITE_CLOSED = "</write>";

	private static final String AVATAR_CLOSED = "</avatar>";

	private static final String AVATAR = "<avatar>";

	private static final String LIST_CLOSED = "</list>";

	private static final String ACE_CLOSED = "</ace>";

	private static final String ENTITYID_CLOSED = "</entityId>";

	private static final String ENTITYID = "<entityId>";

	private static final String ACE = "<ace>";

	private static final String AND_B_LD_TENANTID = " and B.ld_tenantid = ";

	private static final String LIST = "<list>";

	private static final String ENTITY = "<entity><![CDATA[";

	private static final String ENTITY_CLOSED = "]]></entity>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		long id = Long.parseLong(request.getParameter("id"));
		String type = request.getParameter("type");
		switch (type) {
			case "menu":
				menuACL(response, id, session.getTenantId());
				break;
			case "folder":
				folderACL(response, id);
				break;
			case "template":
				templateACL(response, id);
				break;
			case "note":
				noteACL(response, id);
				break;
			default:
				documentACL(response, id);
		}

	}

	/**
	 * Useful method for retrieving the label for the users
	 */
	private Map<Long, String> getUsers(long tenantId) throws PersistenceException {
		Map<Long, String> users = new HashMap<>();
		UserDAO dao = UserDAO.get();
		dao.queryForResultSet(
				"select ld_id, ld_username, ld_firstname, ld_name from ld_user where ld_deleted=0 and ld_tenantid="
						+ tenantId,
				null, null, rows -> {
					while (rows.next())
						users.put(rows.getLong(1),
								rows.getString(3) + " " + rows.getString(4) + " (" + rows.getString(2) + ")");

				});
		return users;
	}

	private void templateACL(HttpServletResponse response, long templateId) throws IOException, PersistenceException {
		TemplateDAO tDao = TemplateDAO.get();
		Template template = tDao.findById(templateId);
		tDao.initialize(template);

		// Prepare a map of users
		Map<Long, String> users = getUsers(template.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write(LIST);

		// Prepare the query on the folder group in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, A.ld_read from ld_template_acl A, ld_group B where A.ld_templateid = ");
		query.append("" + template.getId());
		query.append(AND_B_LD_TENANTID + template.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_name asc");

		tDao.queryForResultSet(query.toString(), null, null, rows -> {
			/*
			 * Iterate over records composing the response XML document
			 */
			while (rows.next()) {
				long groupId = rows.getLong(1);
				String groupName = rows.getString(2);
				int groupType = rows.getInt(3);
				long userId = 0L;
				if (groupType == GroupType.USER.ordinal() && groupName != null)
					userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

				writer.print(ACE);
				writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

				if (groupType == GroupType.DEFAULT.ordinal()) {
					writer.print(ENTITY + groupName + ENTITY_CLOSED);
					writer.print(AVATAR_GROUP_AVATAR);
				} else {
					writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
					writer.print(AVATAR + userId + AVATAR_CLOSED);
				}
				writer.print(WRITE + (rows.getInt(4) == 1) + WRITE_CLOSED);
				writer.print(READ + (rows.getInt(5) == 1) + READ_CLOSED);
				writer.print(TYPE + groupType + TYPE_CLOSED);
				writer.print(ACE_CLOSED);

			}
		});

		writer.write(LIST_CLOSED);
	}

	private void documentACL(HttpServletResponse response, long documentId) throws IOException, PersistenceException {
		DocumentDAO docDao = DocumentDAO.get();
		Document document = docDao.findById(documentId);
		docDao.initialize(document);

		// Prepare a map of users
		Map<Long, String> users = getUsers(document.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write(LIST);

		// Prepare the query on the ACL in join with groups
		StringBuilder query = new StringBuilder("""
						 select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, 0, A.ld_security, A.ld_immutable, A.ld_delete,
						        A.ld_rename, 0, 0, A.ld_sign, A.ld_archive, A.ld_workflow, A.ld_download,
						        A.ld_calendar, A.ld_subscription, A.ld_print, A.ld_password, A.ld_move, A.ld_email, A.ld_automation,
						        0, A.ld_readingreq, A.ld_read, A.ld_preview, A.ld_customid, A.ld_revision
						   from ld_document_acl A, ld_group B 
						  where A.ld_docid =
						""");
		query.append(Long.toString(documentId));
		query.append(AND_B_LD_TENANTID + document.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_type asc, B.ld_name asc");

		docDao.queryForResultSet(query.toString(), null, null, rows -> {
			/*
			 * Iterate over records composing the response XML document
			 */
			while (rows.next())
				printACE(writer, rows, users);
		});

		writer.write(LIST_CLOSED);
	}

	private void folderACL(HttpServletResponse response, long folderId) throws IOException, PersistenceException {
		FolderDAO folderDao = FolderDAO.get();
		Folder folder = folderDao.findById(folderId);
		folderDao.initialize(folder);

		Folder ref = folder;
		if (folder.getSecurityRef() != null) {
			ref = folderDao.findById(folder.getSecurityRef());
			folderDao.initialize(ref);
		}

		// Prepare a map of users
		Map<Long, String> users = getUsers(ref.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write(LIST);

		// Prepare the query on the ACL in join with groups
		StringBuilder query = new StringBuilder("""
						select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, A.ld_add, A.ld_security, A.ld_immutable, A.ld_delete,
						       A.ld_rename, A.ld_import, A.ld_export, A.ld_sign, A.ld_archive, A.ld_workflow, A.ld_download,
						       A.ld_calendar, A.ld_subscription, A.ld_print, A.ld_password, A.ld_move, A.ld_email, A.ld_automation,
						       A.ld_store, A.ld_readingreq, A.ld_read, A.ld_preview, A.ld_customid, A.ld_revision
						  from ld_folder_acl A, ld_group B 
						 where A.ld_folderid =
						""");
		query.append(Long.toString(ref.getId()));
		query.append(AND_B_LD_TENANTID + ref.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_type asc, B.ld_name asc");

		folderDao.queryForResultSet(query.toString(), null, null, rows -> {
			/*
			 * Iterate over records composing the response XML document
			 */
			while (rows.next())
				printACE(writer, rows, users);
		});

		writer.write(LIST_CLOSED);
	}

	private void menuACL(HttpServletResponse response, long menuId, long tenantId)
			throws IOException, PersistenceException {
		MenuDAO menuDao = MenuDAO.get();
		Menu menu = menuDao.findById(menuId);
		menuDao.initialize(menu);

		// Prepare a map of users
		Map<Long, String> users = getUsers(tenantId);

		PrintWriter writer = response.getWriter();
		writer.write(LIST);

		// Prepare the query on the menu ACL in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_read from ld_menu_acl A, ld_group B where A.ld_menuid = ");
		query.append("" + menu.getId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id and B.ld_tenantid = " + tenantId);
		query.append(" order by B.ld_type asc, B.ld_name asc");

		menuDao.queryForResultSet(query.toString(), null, null, rows -> {
			/*
			 * Iterate over records composing the response XML document
			 */
			while (rows.next()) {
				long groupId = rows.getLong(1);
				String groupName = rows.getString(2);
				int groupType = rows.getInt(3);
				long userId = 0L;
				if (groupType == GroupType.USER.ordinal() && groupName != null)
					userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

				writer.print(ACE);
				writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

				if (groupType == GroupType.DEFAULT.ordinal()) {
					writer.print(ENTITY + groupName + ENTITY_CLOSED);
					writer.print(AVATAR_GROUP_AVATAR);
				} else {
					writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
					writer.print(AVATAR + userId + AVATAR_CLOSED);
				}

				writer.print(READ + intToBoolean(rows.getInt(4)) + READ_CLOSED);
				writer.print(TYPE + groupType + TYPE_CLOSED);
				writer.print(ACE_CLOSED);
			}
		});

		writer.write(LIST_CLOSED);
	}

	private void noteACL(HttpServletResponse response, long noteId) throws IOException, PersistenceException {
		DocumentNoteDAO noteDao = DocumentNoteDAO.get();
		DocumentNote note = noteDao.findById(noteId);
		noteDao.initialize(note);

		// Prepare a map of users
		Map<Long, String> users = getUsers(note.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write(LIST);

		// Prepare the query on the menu ACL in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_read, A.ld_write, A.ld_delete, A.ld_security from ld_note_acl A, ld_group B where A.ld_noteid = ");
		query.append("" + note.getId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id and B.ld_tenantid = " + note.getTenantId());
		query.append(" order by B.ld_type asc, B.ld_name asc");

		noteDao.queryForResultSet(query.toString(), null, null, rows -> {
			/*
			 * Iterate over records composing the response XML document
			 */
			while (rows.next()) {
				long groupId = rows.getLong(1);
				String groupName = rows.getString(2);
				int groupType = rows.getInt(3);
				long userId = 0L;
				if (groupType == GroupType.USER.ordinal() && groupName != null)
					userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

				writer.print(ACE);
				writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

				if (groupType == GroupType.DEFAULT.ordinal()) {
					writer.print(ENTITY + groupName + ENTITY_CLOSED);
					writer.print(AVATAR_GROUP_AVATAR);
				} else {
					writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
					writer.print(AVATAR + userId + AVATAR_CLOSED);
				}

				writer.print(READ + intToBoolean(rows.getInt(4)) + READ_CLOSED);
				writer.print(WRITE + intToBoolean(rows.getInt(5)) + WRITE_CLOSED);
				writer.print("<delete>" + intToBoolean(rows.getInt(6)) + "</delete>");
				writer.print("<security>" + intToBoolean(rows.getInt(6)) + "</security>");
				writer.print(TYPE + groupType + TYPE_CLOSED);

				writer.print(ACE_CLOSED);
			}
		});

		writer.write(LIST_CLOSED);
	}

	private void printACE(PrintWriter writer, ResultSet set, Map<Long, String> users) throws SQLException {
		long groupId = set.getLong(1);
		String groupName = set.getString(2);
		int groupType = set.getInt(3);
		long userId = 0L;
		if (groupType == GroupType.USER.ordinal() && groupName != null)
			userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

		writer.print(ACE);
		writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

		if (groupType == GroupType.DEFAULT.ordinal()) {
			writer.print(ENTITY + groupName + ENTITY_CLOSED);
			writer.print(AVATAR_GROUP_AVATAR);
		} else {
			writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
			writer.print(AVATAR + userId + AVATAR_CLOSED);
		}

		printPermission("write", writer, intToBoolean(set.getInt(4)));
		printPermission("add", writer, intToBoolean(set.getInt(5)));
		printPermission("security", writer, intToBoolean(set.getInt(6)));
		printPermission("immutable", writer, intToBoolean(set.getInt(7)));
		printPermission("delete", writer, intToBoolean(set.getInt(8)));
		printPermission("rename", writer, intToBoolean(set.getInt(9)));
		printPermission("import", writer, intToBoolean(set.getInt(10)));
		printPermission("export", writer, intToBoolean(set.getInt(11)));
		printPermission("sign", writer, intToBoolean(set.getInt(12)));
		printPermission("archive", writer, intToBoolean(set.getInt(13)));
		printPermission("workflow", writer, intToBoolean(set.getInt(14)));
		printPermission("download", writer, intToBoolean(set.getInt(15)));
		printPermission("calendar", writer, intToBoolean(set.getInt(16)));
		printPermission("subscription", writer, intToBoolean(set.getInt(17)));
		printPermission("print", writer, intToBoolean(set.getInt(18)));
		printPermission("password", writer, intToBoolean(set.getInt(19)));
		printPermission("move", writer, intToBoolean(set.getInt(20)));
		printPermission("email", writer, intToBoolean(set.getInt(21)));
		printPermission("automation", writer, intToBoolean(set.getInt(22)));
		printPermission("store", writer, intToBoolean(set.getInt(23)));
		printPermission("readingreq", writer, intToBoolean(set.getInt(24)));
		printPermission("read", writer, intToBoolean(set.getInt(25)));
		printPermission("preview", writer, intToBoolean(set.getInt(26)));
		printPermission("customid", writer, intToBoolean(set.getInt(27)));
		printPermission("revision", writer, intToBoolean(set.getInt(28)));
		
		writer.print(TYPE + groupType + TYPE_CLOSED);
		writer.print(ACE_CLOSED);
	}

	private void printPermission(String permission, PrintWriter writer, boolean enabled) throws SQLException {
		writer.print(String.format("<%s>%b</%s>", permission, enabled, permission));
	}

	private boolean intToBoolean(int val) {
		return val == 1;
	}
}