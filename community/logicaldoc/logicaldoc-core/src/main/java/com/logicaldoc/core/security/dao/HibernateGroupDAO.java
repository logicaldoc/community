package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Collection;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateGroupDAO extends HibernatePersistentObjectDAO<Group> implements GroupDAO {

	private MenuDAO menuDAO;

	private HibernateGroupDAO() {
		super(Group.class);
		super.log = LoggerFactory.getLogger(HibernateGroupDAO.class);
	}

	public MenuDAO getMenuDAO() {
		return menuDAO;
	}

	public void setMenuDAO(MenuDAO menuDAO) {
		this.menuDAO = menuDAO;
	}

	public boolean delete(long groupId, int code) {
		assert (code != 0);
		boolean result = true;

		try {
			Group group = findById(groupId);
			refresh(group);
			if (group != null) {
				group.setName(group.getName() + "." + group.getId());
				group.setDeleted(code);
				saveOrUpdate(group);
			}
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	public boolean exists(String groupname, long tenantId) {
		boolean result = false;

		try {
			Group group = findByName(groupname, tenantId);
			result = (group != null);
		} catch (Exception e) {
			if (log.isWarnEnabled())
				log.warn(e.getMessage());
		}

		return result;
	}

	@Override
	public Collection<String> findAllGroupNames(long tenantId) {
		Collection<String> coll = new ArrayList<String>();

		try {
			Collection<Group> coll2 = findAll();
			for (Group group : coll2) {
				if (group.getTenantId() == tenantId)
					coll.add(group.getName());
			}
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage());
		}

		return coll;
	}

	/**
	 * @see com.logicaldoc.core.security.dao.GroupDAO#findByName(java.lang.String)
	 */
	public Group findByName(String name, long tenantId) {
		Group group = null;
		Collection<Group> coll = findByWhere(
				"_entity.tenantId=" + tenantId + " and _entity.name = '" + SqlUtil.doubleQuotes(name) + "'", null, null);
		if (coll.size() > 0) {
			group = coll.iterator().next();
			if (group.getDeleted() == 1)
				group = null;
		}
		return group;
	}

	/**
	 * @see com.logicaldoc.core.security.dao.GroupDAO#insert(com.logicaldoc.core.security.Group,
	 *      long)
	 */
	public boolean insert(Group group, long parentGroupId) {
		boolean result = true;

		if (group == null)
			return false;

		try {
			saveOrUpdate(group);
			flush();

			if (parentGroupId != 0) {
				// Inherit ACLs from the parent group
				inheritACLs(group.getId(), parentGroupId);
			}
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public void inheritACLs(long groupId, long parentGroupId) {
		if (groupId == parentGroupId)
			return;

		try {
			String sql = "delete from ld_menugroup where ld_groupid=" + groupId;
			log.debug("Delete all menugroup for group " + groupId);
			jdbcUpdate(sql);

			sql = "delete from ld_foldergroup where ld_groupid=" + groupId;
			log.debug("Delete all foldergroup for group " + groupId);
			jdbcUpdate(sql);

			if (parentGroupId != Group.GROUPID_ADMIN) {
				log.debug("Replicate all ACLs from group " + parentGroupId + " to group " + groupId);

				sql = "insert into ld_menugroup(ld_menuid, ld_groupid, ld_write) " + "select B.ld_menuid," + groupId
						+ ", B.ld_write from ld_menugroup B where B.ld_groupid= " + parentGroupId;
				log.debug("Replicate all ACLs from group " + parentGroupId);
				jdbcUpdate(sql);

				sql = "insert into ld_foldergroup(ld_folderid, ld_groupid, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email) "
						+ "select B.ld_folderid,"
						+ groupId
						+ ", B.ld_write, B.ld_add, B.ld_security, B.ld_immutable, B.ld_delete, B.ld_rename, B.ld_import, B.ld_export, B.ld_sign, B.ld_archive, B.ld_workflow, B.ld_download, B.ld_calendar, B.ld_subscription, B.ld_print, B.ld_password, B.ld_move, B.ld_email from ld_foldergroup B "
						+ "where B.ld_groupid= " + parentGroupId;
				jdbcUpdate(sql);
			} else {
				// Inheriting from admin means access everything
				log.debug("Replicate all admin ACLs to group " + groupId);

				sql = "insert into ld_menugroup(ld_menuid, ld_groupid, ld_write) select B.ld_id," + groupId
						+ ",1 from ld_menu B where B.ld_deleted=0";
				jdbcUpdate(sql);

				sql = "insert into ld_foldergroup(ld_folderid, ld_groupid, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email) "
						+ "select B.ld_id,"
						+ groupId
						+ ",1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 from ld_folder B "
						+ "where B.ld_deleted=0";
				jdbcUpdate(sql);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public Collection<Group> findByLikeName(String name, long tenantId) {
		return findByWhere("lower(_entity.name) like ?1 and _entity.tenantId = ?2", new Object[] { name.toLowerCase(),
				tenantId }, null, null);
	}

	@Override
	public int count() {
		String query = "select count(*) from ld_group where ld_deleted=0";
		return queryForInt(query);
	}

	@Override
	public void initialize(Group group) {
		if (group != null && group.getDeleted() == 0)
			refresh(group);

		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		group.setUsers(uDao.findByGroup(group.getId()));
	}
}