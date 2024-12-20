package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;

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
			menuAcl(response, id, session.getTenantId());
			break;
		case "folder":
			folderACL(response, id);
			break;
		case "template":
			templateACL(response, id);
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
		UserDAO dao = Context.get(UserDAO.class);
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
		TemplateDAO tDao = Context.get(TemplateDAO.class);
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
				if (groupType == Group.TYPE_USER && groupName != null)
					userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

				writer.print(ACE);
				writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

				if (groupType == Group.TYPE_DEFAULT) {
					writer.print(ENTITY + groupName + ENTITY_CLOSED);
					writer.print(AVATAR_GROUP_AVATAR);
				} else {
					writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
					writer.print(AVATAR + userId + AVATAR_CLOSED);
				}
				writer.print("<write>" + (rows.getInt(4) == 1) + "</write>");
				writer.print(READ + (rows.getInt(5) == 1) + READ_CLOSED);
				writer.print(TYPE + groupType + TYPE_CLOSED);
				writer.print(ACE_CLOSED);

			}
		});

		writer.write(LIST_CLOSED);
	}

	private void documentACL(HttpServletResponse response, long documentId) throws IOException, PersistenceException {
		DocumentDAO docDao = Context.get(DocumentDAO.class);
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
						        0, A.ld_readingreq, A.ld_read, A.ld_preview, A.ld_customid
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
		FolderDAO folderDao = Context.get(FolderDAO.class);
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
						       A.ld_store, A.ld_readingreq, A.ld_read, A.ld_preview, A.ld_customid
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

	private void menuAcl(HttpServletResponse response, long menuId, long tenantId)
			throws IOException, PersistenceException {
		MenuDAO menuDao = Context.get(MenuDAO.class);
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
				if (groupType == Group.TYPE_USER && groupName != null)
					userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

				writer.print(ACE);
				writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

				if (groupType == Group.TYPE_DEFAULT) {
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

	private void printACE(PrintWriter writer, ResultSet set, Map<Long, String> users) throws SQLException {
		long groupId = set.getLong(1);
		String groupName = set.getString(2);
		int groupType = set.getInt(3);
		long userId = 0L;
		if (groupType == Group.TYPE_USER && groupName != null)
			userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

		writer.print(ACE);
		writer.print(ENTITYID + groupId + ENTITYID_CLOSED);

		if (groupType == Group.TYPE_DEFAULT) {
			writer.print(ENTITY + groupName + ENTITY_CLOSED);
			writer.print(AVATAR_GROUP_AVATAR);
		} else {
			writer.print(ENTITY + users.get(userId) + ENTITY_CLOSED);
			writer.print(AVATAR + userId + AVATAR_CLOSED);
		}

		writer.print("<write>" + intToBoolean(set.getInt(4)) + "</write>");
		writer.print("<add>" + intToBoolean(set.getInt(5)) + "</add>");
		writer.print("<security>" + intToBoolean(set.getInt(6)) + "</security>");
		writer.print("<immutable>" + intToBoolean(set.getInt(7)) + "</immutable>");
		writer.print("<delete>" + intToBoolean(set.getInt(8)) + "</delete>");
		writer.print("<rename>" + intToBoolean(set.getInt(9)) + "</rename>");
		writer.print("<import>" + intToBoolean(set.getInt(10)) + "</import>");
		writer.print("<export>" + intToBoolean(set.getInt(11)) + "</export>");
		writer.print("<sign>" + intToBoolean(set.getInt(12)) + "</sign>");
		writer.print("<archive>" + intToBoolean(set.getInt(13)) + "</archive>");
		writer.print("<workflow>" + intToBoolean(set.getInt(14)) + "</workflow>");
		writer.print("<download>" + intToBoolean(set.getInt(15)) + "</download>");
		writer.print("<calendar>" + intToBoolean(set.getInt(16)) + "</calendar>");
		writer.print("<subscription>" + intToBoolean(set.getInt(17)) + "</subscription>");
		writer.print("<print>" + intToBoolean(set.getInt(18)) + "</print>");
		writer.print("<password>" + intToBoolean(set.getInt(19)) + "</password>");
		writer.print("<move>" + intToBoolean(set.getInt(20)) + "</move>");
		writer.print("<email>" + intToBoolean(set.getInt(21)) + "</email>");
		writer.print("<automation>" + intToBoolean(set.getInt(22)) + "</automation>");
		writer.print("<store>" + intToBoolean(set.getInt(23)) + "</store>");
		writer.print("<readingreq>" + intToBoolean(set.getInt(24)) + "</readingreq>");
		writer.print(READ + intToBoolean(set.getInt(25)) + READ_CLOSED);
		writer.print("<preview>" + intToBoolean(set.getInt(26)) + "</preview>");
		writer.print("<customid>" + intToBoolean(set.getInt(27)) + "</customid>");
		writer.print(TYPE + groupType + TYPE_CLOSED);
		writer.print(ACE_CLOSED);
	}

	private boolean intToBoolean(int val) {
		return val == 1;
	}
}