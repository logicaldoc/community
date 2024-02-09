package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.jdbc.support.rowset.SqlRowSet;

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
 * This servlet is responsible retrieving the Access Control List for documents, folders, menus and templates
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class AclDataServlet extends AbstractDataServlet {

	private static final String ENTITY = "<entity><![CDATA[";

	private static final String CLOSE_ENTITY = "]]></entity>";

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
		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		SqlRowSet set = dao.queryForRowSet(
				"select ld_id, ld_username, ld_firstname, ld_name from ld_user where ld_deleted=0 and ld_tenantid="
						+ tenantId,
				null);
		Map<Long, String> users = new HashMap<>();
		while (set.next())
			users.put(set.getLong(1), set.getString(3) + " " + set.getString(4) + " (" + set.getString(2) + ")");
		return users;
	}
	
	private void templateACL(HttpServletResponse response, long templateId)
			throws IOException, PersistenceException {
		TemplateDAO tDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
		Template template = tDao.findById(templateId);
		tDao.initialize(template);

		// Prepare a map of users
		Map<Long, String> users = getUsers(template.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the folder group in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, A.ld_read from ld_template_acl as A, ld_group B where A.ld_templateid = ");
		query.append("" + template.getId());
		query.append(" and B.ld_tenantid = " + template.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_name asc");

		SqlRowSet set = tDao.queryForRowSet(query.toString(), null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next()) {
			long groupId = set.getLong(1);
			String groupName = set.getString(2);
			int groupType = set.getInt(3);
			long userId = 0L;
			if (groupType == Group.TYPE_USER && groupName != null)
				userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

			writer.print("<ace>");
			writer.print("<entityId>" + groupId + "</entityId>");

			if (groupType == Group.TYPE_DEFAULT) {
				writer.print("<entity><![CDATA[" + groupName + "]]></entity>");
				writer.print("<avatar>group</avatar>");
			} else {
				writer.print("<entity><![CDATA[" + users.get(userId) + "]]></entity>");
				writer.print("<avatar>" + userId + "</avatar>");
			}
			writer.print("<write>" + (set.getInt(4) == 1) + "</write>");
			writer.print("<read>" + (set.getInt(5) == 1) + "</read>");
			writer.print("<type>" + groupType + "</type>");
			writer.print("</ace>");

		}

		writer.write("</list>");
	}

	private void documentACL(HttpServletResponse response, long documentId)
			throws IOException, PersistenceException {
		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Document document = docDao.findById(documentId);
		docDao.initialize(document);

		// Prepare a map of users
		Map<Long, String> users = getUsers(document.getTenantId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the ACL in join with groups
		StringBuilder query = new StringBuilder(
				"""
						select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, 0, A.ld_security, A.ld_immutable, A.ld_delete,
						       A.ld_rename, 0, 0, A.ld_sign, A.ld_archive, A.ld_workflow, A.ld_download,
						       A.ld_calendar, A.ld_subscription, A.ld_print, A.ld_password, A.ld_move, A.ld_email, A.ld_automation,
						       0, A.ld_readingreq, A.ld_read
						  from ld_document_acl A, ld_group B 
						 where A.ld_docid =
						""");
		query.append(Long.toString(documentId));
		query.append(" and B.ld_tenantid = " + document.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_type asc, B.ld_name asc");

		SqlRowSet set = docDao.queryForRowSet(query.toString(), null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next())
			printACE(writer, set, users);

		writer.write("</list>");
	}

	private void folderACL(HttpServletResponse response, long folderId) throws IOException, PersistenceException {
		FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
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
		writer.write("<list>");

		// Prepare the query on the ACL in join with groups
		StringBuilder query = new StringBuilder(
				"""
						select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, A.ld_add, A.ld_security, A.ld_immutable, A.ld_delete,
						       A.ld_rename, A.ld_import, A.ld_export, A.ld_sign, A.ld_archive, A.ld_workflow, A.ld_download,
						       A.ld_calendar, A.ld_subscription, A.ld_print, A.ld_password, A.ld_move, A.ld_email, A.ld_automation,
						       A.ld_storage, A.ld_readingreq, A.ld_read
						  from ld_folder_acl A, ld_group B 
						 where A.ld_folderid =
						""");
		query.append(Long.toString(ref.getId()));
		query.append(" and B.ld_tenantid = " + ref.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_type asc, B.ld_name asc");

		SqlRowSet set = folderDao.queryForRowSet(query.toString(), null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next())
			printACE(writer, set, users);

		writer.write("</list>");
	}

	private void menuAcl(HttpServletResponse response, long menuId, long tenantId)
			throws IOException, PersistenceException {
		MenuDAO menuDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		Menu menu = menuDao.findById(menuId);
		menuDao.initialize(menu);

		// Prepare a map of users
		Map<Long, String> users = getUsers(tenantId);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the menu ACL in join with groups
		StringBuilder query = new StringBuilder(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_read from ld_menu_acl A, ld_group B where A.ld_menuid = ");
		query.append("" + menu.getId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id and B.ld_tenantid = " + tenantId);
		query.append(" order by B.ld_type asc, B.ld_name asc");

		SqlRowSet set = menuDao.queryForRowSet(query.toString(), null);

		/*
		 * Iterate over records composing the response XML document
		 */
		while (set.next()) {
			long groupId = set.getLong(1);
			String groupName = set.getString(2);
			int groupType = set.getInt(3);
			long userId = 0L;
			if (groupType == Group.TYPE_USER && groupName != null)
				userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

			writer.print("<ace>");
			writer.print("<entityId>" + groupId + "</entityId>");

			if (groupType == Group.TYPE_DEFAULT) {
				writer.print(ENTITY + groupName + CLOSE_ENTITY);
				writer.print("<avatar>group</avatar>");
			} else {
				writer.print(ENTITY + users.get(userId) + CLOSE_ENTITY);
				writer.print("<avatar>" + userId + "</avatar>");
			}

			writer.print("<read>" + intToBoolean(set.getInt(4)) + "</read>");
			writer.print("<type>" + groupType + "</type>");
			writer.print("</ace>");
		}

		writer.write("</list>");
	}

	private void printACE(PrintWriter writer, SqlRowSet set, Map<Long, String> users) {
		long groupId = set.getLong(1);
		String groupName = set.getString(2);
		int groupType = set.getInt(3);
		long userId = 0L;
		if (groupType == Group.TYPE_USER && groupName != null)
			userId = Long.parseLong(groupName.substring(groupName.lastIndexOf('_') + 1));

		writer.print("<ace>");
		writer.print("<entityId>" + groupId + "</entityId>");

		if (groupType == Group.TYPE_DEFAULT) {
			writer.print(ENTITY + groupName + CLOSE_ENTITY);
			writer.print("<avatar>group</avatar>");
		} else {
			writer.print(ENTITY + users.get(userId) + CLOSE_ENTITY);
			writer.print("<avatar>" + userId + "</avatar>");
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
		writer.print("<storage>" + intToBoolean(set.getInt(23)) + "</storage>");
		writer.print("<readingreq>" + intToBoolean(set.getInt(24)) + "</readingreq>");
		writer.print("<read>" + intToBoolean(set.getInt(25)) + "</read>");
		writer.print("<type>" + groupType + "</type>");
		writer.print("</ace>");
	}

	private boolean intToBoolean(int val) {
		return val == 1;
	}
}