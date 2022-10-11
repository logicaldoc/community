package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
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
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for rights data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class RightsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale) throws PersistenceException, IOException {

		Long folderId = null;
		if (StringUtils.isNotEmpty(request.getParameter("folderId")))
			folderId = Long.parseLong(request.getParameter("folderId"));

		Long menuId = null;
		if (StringUtils.isNotEmpty(request.getParameter("menuId")))
			menuId = Long.parseLong(request.getParameter("menuId"));

		if (folderId != null)
			folderRights(response, folderId);
		else
			menuRights(response, menuId, session.getTenantId());
	}

	/**
	 * Useful method for retrieving the label for the users
	 */
	private Map<Long, String> getUsers(long tenantId) throws PersistenceException {
		UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
		SqlRowSet set = dao.queryForRowSet(
				"select ld_id, ld_username, ld_firstname, ld_name from ld_user where ld_deleted=0 and ld_tenantid="
						+ tenantId,
				null, null);
		Map<Long, String> users = new HashMap<Long, String>();
		while (set.next())
			users.put(set.getLong(1), set.getString(3) + " " + set.getString(4) + " (" + set.getString(2) + ")");
		return users;
	}

	private void folderRights(HttpServletResponse response, Long folderId) throws IOException, PersistenceException {
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

		// Prepare the query on the folder group in join with groups
		StringBuffer query = new StringBuffer(
				"select A.ld_groupid, B.ld_name, B.ld_type, A.ld_write, A.ld_add, A.ld_security, A.ld_immutable, A.ld_delete, A.ld_rename, A.ld_import, A.ld_export, A.ld_sign, A.ld_archive, A.ld_workflow, A.ld_download, ");
		query.append(
				" A.ld_calendar, A.ld_subscription, A.ld_print, A.ld_password, A.ld_move, A.ld_email, A.ld_automation, A.ld_storage from ld_foldergroup as A, ld_group B where A.ld_folderid = ");
		query.append("" + ref.getId());
		query.append(" and B.ld_tenantid = " + ref.getTenantId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id order by B.ld_type asc, B.ld_name asc");

		SqlRowSet set = folderDao.queryForRowSet(query.toString(), null, null);

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

			writer.print("<right>");
			writer.print("<entityId>" + groupId + "</entityId>");

			if (groupType == Group.TYPE_DEFAULT) {
				writer.print("<entity><![CDATA[" + groupName + "]]></entity>");
				writer.print("<avatar>group</avatar>");
			} else {
				writer.print("<entity><![CDATA[" + users.get(userId) + "]]></entity>");
				writer.print("<avatar>" + userId + "</avatar>");
			}
			writer.print("<read>true</read>");
			writer.print("<write>" + (set.getInt(4) == 1 ? true : false) + "</write>");
			writer.print("<add>" + (set.getInt(5) == 1 ? true : false) + "</add>");
			writer.print("<security>" + (set.getInt(6) == 1 ? true : false) + "</security>");
			writer.print("<immutable>" + (set.getInt(7) == 1 ? true : false) + "</immutable>");
			writer.print("<delete>" + (set.getInt(8) == 1 ? true : false) + "</delete>");
			writer.print("<rename>" + (set.getInt(9) == 1 ? true : false) + "</rename>");
			writer.print("<import>" + (set.getInt(10) == 1 ? true : false) + "</import>");
			writer.print("<export>" + (set.getInt(11) == 1 ? true : false) + "</export>");
			writer.print("<sign>" + (set.getInt(12) == 1 ? true : false) + "</sign>");
			writer.print("<archive>" + (set.getInt(13) == 1 ? true : false) + "</archive>");
			writer.print("<workflow>" + (set.getInt(14) == 1 ? true : false) + "</workflow>");
			writer.print("<download>" + (set.getInt(15) == 1 ? true : false) + "</download>");
			writer.print("<calendar>" + (set.getInt(16) == 1 ? true : false) + "</calendar>");
			writer.print("<subscription>" + (set.getInt(17) == 1 ? true : false) + "</subscription>");
			writer.print("<print>" + (set.getInt(18) == 1 ? true : false) + "</print>");
			writer.print("<password>" + (set.getInt(19) == 1 ? true : false) + "</password>");
			writer.print("<move>" + (set.getInt(20) == 1 ? true : false) + "</move>");
			writer.print("<email>" + (set.getInt(21) == 1 ? true : false) + "</email>");
			writer.print("<automation>" + (set.getInt(22) == 1 ? true : false) + "</automation>");
			writer.print("<storage>" + (set.getInt(23) == 1 ? true : false) + "</storage>");
			writer.print("<type>" + groupType + "</type>");
			writer.print("</right>");

		}

		writer.write("</list>");
	}

	private void menuRights(HttpServletResponse response, Long menuId, long tenantId)
			throws IOException, PersistenceException {
		MenuDAO menuDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
		Menu menu = menuDao.findById(menuId);
		menuDao.initialize(menu);

		// Prepare a map of users
		Map<Long, String> users = getUsers(tenantId);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		// Prepare the query on the folder group in join with groups
		StringBuffer query = new StringBuffer(
				"select A.ld_groupid, B.ld_name, B.ld_type from ld_menugroup as A, ld_group B where A.ld_menuid = ");
		query.append("" + menu.getId());
		query.append(" and B.ld_deleted=0 and A.ld_groupid = B.ld_id and B.ld_tenantid = " + tenantId);
		query.append(" order by B.ld_type asc, B.ld_name asc");

		SqlRowSet set = menuDao.queryForRowSet(query.toString(), null, null);

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

			writer.print("<right>");
			writer.print("<entityId>" + groupId + "</entityId>");

			if (groupType == Group.TYPE_DEFAULT) {
				writer.print("<entity><![CDATA[" + groupName + "]]></entity>");
				writer.print("<avatar>group</avatar>");
			} else {
				writer.print("<entity><![CDATA[" + users.get(userId) + "]]></entity>");
				writer.print("<avatar>" + userId + "</avatar>");
			}

			writer.print("<type>" + groupType + "</type>");
			writer.print("</right>");
		}

		writer.write("</list>");
	}
}