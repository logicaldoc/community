package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
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

	@Override
	public void delete(long groupId, int code) throws PersistenceException {
		if (code == 0)
			throw new IllegalArgumentException("code cannot be 0");
		Group group = findById(groupId);
		refresh(group);

		if (group.getName().equals("admin") || group.getName().equals("guest") || group.getName().equals("publisher"))
			throw new PersistenceException(String.format("Group %s cannot be deleted", group.getName()));

		group.setName(group.getName() + "." + group.getId());
		group.setDeleted(code);
		saveOrUpdate(group);
	}

	public boolean exists(String groupname, long tenantId) {
		boolean result = false;

		try {
			Group group = findByName(groupname, tenantId);
			result = (group != null);
		} catch (Exception e) {
			log.warn(e.getMessage());
		}

		return result;
	}

	@Override
	public Collection<String> findAllGroupNames(long tenantId) {
		Collection<String> coll = new ArrayList<>();

		try {
			Collection<Group> coll2 = findAll();
			for (Group group : coll2) {
				if (group.getTenantId() == tenantId)
					coll.add(group.getName());
			}
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		return coll;
	}

	@Override
	public Group findByName(String name, long tenantId) {
		Group group = null;
		try {
			Collection<Group> coll = findByWhere(ENTITY + ".tenantId=" + tenantId + " and " + ENTITY + ".name = '"
					+ SqlUtil.doubleQuotes(name) + "'", null, null);
			if (coll.size() > 0) {
				group = coll.iterator().next();
				if (group.getDeleted() == 1)
					group = null;
			}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return group;
	}

	@Override
	public boolean insert(Group group, long parentGroupId) {
		if (!checkStoringAspect())
			return false;

		boolean result = true;

		if (group == null)
			return false;

		try {
			saveOrUpdate(group);
			flush();

			if (parentGroupId != 0) {
				// Inherit ACLs from the parent group
				inheritACLs(group, parentGroupId);
			} else {
				fixGuestPermissions(group);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public void inheritACLs(Group group, long parentGroupId) {
		long groupId = group.getId();
		if (groupId == parentGroupId)
			return;

		try {
			String sql = "delete from ld_menugroup where ld_groupid=" + groupId;
			log.debug("Delete all menugroup for group {}", groupId);
			jdbcUpdate(sql);

			sql = "delete from ld_foldergroup where ld_groupid=" + groupId;
			log.debug("Delete all foldergroup for group {}", groupId);
			jdbcUpdate(sql);

			if (parentGroupId != Group.GROUPID_ADMIN) {
				log.debug("Replicate all ACLs from group {} to group {}", parentGroupId, groupId);

				sql = "insert into ld_menugroup(ld_menuid, ld_groupid, ld_write) select B.ld_menuid," + groupId
						+ ", B.ld_write from ld_menugroup B where B.ld_groupid= " + parentGroupId;
				log.debug("Replicate all ACLs from group {}", parentGroupId);
				jdbcUpdate(sql);

				sql = "insert into ld_foldergroup(ld_folderid, ld_groupid, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_storage) "
						+ "select B.ld_folderid," + groupId
						+ ", B.ld_write, B.ld_add, B.ld_security, B.ld_immutable, B.ld_delete, B.ld_rename, B.ld_import, B.ld_export, B.ld_sign, B.ld_archive, B.ld_workflow, B.ld_download, B.ld_calendar, B.ld_subscription, B.ld_print, B.ld_password, B.ld_move, B.ld_email, B.ld_automation, B.ld_storage from ld_foldergroup B "
						+ "where B.ld_groupid= " + parentGroupId;
				jdbcUpdate(sql);
			} else {
				// Inheriting from admin means access everything
				log.debug("Replicate all admin ACLs to group {}", groupId);

				sql = "insert into ld_menugroup(ld_menuid, ld_groupid, ld_write) select B.ld_id," + groupId
						+ ",1 from ld_menu B where B.ld_deleted=0";
				jdbcUpdate(sql);

				sql = "insert into ld_foldergroup(ld_folderid, ld_groupid, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_storage) "
						+ "select B.ld_id," + groupId + ",1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 from ld_folder B "
						+ "where B.ld_deleted=0";
				jdbcUpdate(sql);
			}

			fixGuestPermissions(group);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public Collection<Group> findByLikeName(String name, long tenantId) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put("name", name.toLowerCase());
			params.put("tenantId", tenantId);
			return findByWhere("lower(" + ENTITY + ".name) like :name and " + ENTITY + ".tenantId = :tenantId", params,
					null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public int count() {
		String query = "select count(*) from ld_group where ld_deleted=0";
		try {
			return queryForInt(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public void initialize(Group group) {
		if (group == null)
			return;

		if (group.getDeleted() == 0)
			refresh(group);

		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		group.setUsers(uDao.findByGroup(group.getId()));
	}

	@Override
	public void store(Group group) throws PersistenceException {
		super.store(group);
		fixGuestPermissions(group);
	}

	/**
	 * If the user is guest, we remove not admitted permissions
	 */
	@Override
	public void fixGuestPermissions(Group group) {
		if (group == null)
			return;
		try {
			boolean guest = group.isGuest();
			if (!guest && group.isUserGroup()) {
				UserDAO userDao = (UserDAO) Context.get().getBean(UserDAO.class);
				User user = userDao
						.findById(Long.parseLong(group.getName().substring(group.getName().lastIndexOf('_') + 1)));
				guest = user.isReadonly();
			}

			if (guest) {
				// Remove not admitted permissions in folders
				String sql = "update ld_foldergroup set ld_write=0, ld_add=0, ld_security=0, ld_immutable=0, ld_delete=0, ld_rename=0, ld_import=0, ld_export=0,"
						+ " ld_sign=0, ld_archive=0, ld_workflow=0, ld_calendar=0, ld_password=0, ld_move=0, ld_automation=0 , ld_storage=0 "
						+ " where ld_groupid=" + group.getId();
				jdbcUpdate(sql);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}
}