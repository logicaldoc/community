package com.logicaldoc.core.security.user;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.annotation.Resource;

/**
 * Hibernate implementation of {@link GroupDAO}
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.0
 */
@Repository("groupDAO")
@Transactional
public class HibernateGroupDAO extends HibernatePersistentObjectDAO<Group> implements GroupDAO {

	@Resource(name = "menuDAO")
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

	public boolean exists(String groupname, long tenantId) throws PersistenceException {
		return findByName(groupname, tenantId) != null;
	}

	@Override
	public Collection<String> findAllGroupNames(long tenantId) throws PersistenceException {
		Collection<String> coll = new ArrayList<>();
		Collection<Group> coll2 = findAll();
		for (Group group : coll2) {
			if (group.getTenantId() == tenantId)
				coll.add(group.getName());
		}
		return coll;
	}

	@Override
	public Group findByName(String name, long tenantId) throws PersistenceException {
		Group group = null;
		Collection<Group> coll = findByWhere(
				ENTITY + ".tenantId=" + tenantId + " and " + ENTITY + ".name = '" + SqlUtil.doubleQuotes(name) + "'",
				null, null);
		if (CollectionUtils.isNotEmpty(coll)) {
			group = coll.iterator().next();
			if (group.getDeleted() == 1)
				group = null;
		}
		return group;
	}

	@Override
	public void insert(Group group, long parentGroupId) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		saveOrUpdate(group);
		flush();

		if (parentGroupId != 0) {
			// Inherit ACLs from the parent group
			inheritACLs(group, parentGroupId);
		} else {
			fixGuestPermissions(group);
		}
	}

	@Override
	public void inheritACLs(Group group, long parentGroupId) throws PersistenceException {
		long groupId = group.getId();
		if (groupId == parentGroupId)
			return;

		String sql = "delete from ld_menu_acl where ld_groupid=" + groupId;
		log.debug("Delete all menugroup for group {}", groupId);
		jdbcUpdate(sql);

		sql = "delete from ld_folder_acl where ld_groupid=" + groupId;
		log.debug("Delete all foldergroup for group {}", groupId);
		jdbcUpdate(sql);

		sql = "delete from ld_document_acl where ld_groupid=" + groupId;
		log.debug("Delete all document ACL for group {}", groupId);
		jdbcUpdate(sql);

		log.debug("Replicate all ACLs from group {} to group {}", parentGroupId, groupId);

		sql = "insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select B.ld_menuid," + groupId
				+ ", B.ld_read, B.ld_write from ld_menu_acl B where B.ld_groupid= " + parentGroupId;
		log.debug("Replicate all ACLs from group {}", parentGroupId);
		jdbcUpdate(sql);

		sql = "insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_preview, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_customid, ld_revision) "
				+ "select B.ld_folderid," + groupId
				+ ", B.ld_read, B.ld_preview, B.ld_write, B.ld_add, B.ld_security, B.ld_immutable, B.ld_delete, B.ld_rename, B.ld_import, B.ld_export, B.ld_sign, B.ld_archive, B.ld_workflow, B.ld_download, B.ld_calendar, B.ld_subscription, B.ld_print, B.ld_password, B.ld_move, B.ld_email, B.ld_automation, B.ld_store, B.ld_readingreq, B.ld_customid, B.ld_revision from ld_folder_acl B "
				+ "where B.ld_groupid= " + parentGroupId;
		jdbcUpdate(sql);

		sql = "insert into ld_document_acl(ld_docid, ld_groupid, ld_read, ld_preview, ld_write , ld_security, ld_immutable, ld_delete, ld_rename, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_readingreq, ld_customid, ld_revision) "
				+ "select B.ld_docid," + groupId
				+ ", B.ld_read, B.ld_preview, B.ld_write, B.ld_security, B.ld_immutable, B.ld_delete, B.ld_rename, B.ld_sign, B.ld_archive, B.ld_workflow, B.ld_download, B.ld_calendar, B.ld_subscription, B.ld_print, B.ld_password, B.ld_move, B.ld_email, B.ld_automation, B.ld_readingreq, B.ld_customid, B.ld_revision from ld_document_acl B "
				+ "where B.ld_groupid= " + parentGroupId;
		jdbcUpdate(sql);

		fixGuestPermissions(group);

	}

	@Override
	public Collection<Group> findByLikeName(String name, long tenantId) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("name", name.toLowerCase());
		params.put("tenantId", tenantId);
		return findByWhere("lower(" + ENTITY + ".name) like :name and " + ENTITY + ".tenantId = :tenantId", params,
				null, null);
	}

	@Override
	public int count() throws PersistenceException {
		return queryForInt("select count(*) from ld_group where ld_deleted=0");
	}

	@Override
	public void initialize(Group group) {
		if (group == null)
			return;

		if (group.getDeleted() == 0)
			refresh(group);

		UserDAO uDao = UserDAO.get();
		try {
			group.setUsers(uDao.findByGroup(group.getId()));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public void store(Group group) throws PersistenceException {
		super.store(group);
		fixGuestPermissions(group);
	}

	/**
	 * If the user is guest, we remove not admitted permissions
	 * 
	 * @throws PersistenceException Error in the DB
	 * @throws NumberFormatException
	 */
	@Override
	public void fixGuestPermissions(Group group) throws PersistenceException {
		if (group == null)
			return;
		boolean guest = group.isGuest();
		if (!guest && group.isUserGroup()) {
			UserDAO userDao = UserDAO.get();
			User user = userDao
					.findById(Long.parseLong(group.getName().substring(group.getName().lastIndexOf('_') + 1)));
			guest = user.isReadonly();
		}

		if (guest) {
			// Remove not admitted permissions in folders
			String sql = "update ld_folder_acl set ld_write=0, ld_add=0, ld_security=0, ld_immutable=0, ld_delete=0, ld_rename=0, ld_import=0, ld_export=0,"
					+ " ld_sign=0, ld_archive=0, ld_workflow=0, ld_calendar=0, ld_password=0, ld_move=0, ld_automation=0 , ld_store=0 "
					+ " where ld_groupid=" + group.getId();
			jdbcUpdate(sql);
			
			// Remove not admitted permissions in documents
			sql = "update ld_document_acl set ld_write=0, ld_security=0, ld_immutable=0, ld_delete=0, ld_rename=0,"
					+ " ld_sign=0, ld_workflow=0, ld_calendar=0, ld_password=0, ld_move=0, ld_automation=0"
					+ " where ld_groupid=" + group.getId();
			jdbcUpdate(sql);
		}
	}
}