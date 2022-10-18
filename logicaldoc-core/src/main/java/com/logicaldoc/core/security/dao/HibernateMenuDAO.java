package com.logicaldoc.core.security.dao;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.MenuGroup;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>MenuDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateMenuDAO extends HibernatePersistentObjectDAO<Menu> implements MenuDAO {

	private UserDAO userDAO;

	protected HibernateMenuDAO() {
		super(Menu.class);
		super.log = LoggerFactory.getLogger(HibernateMenuDAO.class);
	}

	public UserDAO getUserDAO() {
		return userDAO;
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	@Override
	public boolean store(Menu menu) throws PersistenceException {
		boolean result = true;

		try {
			if (menu.getSecurityRef() != null)
				menu.getMenuGroups().clear();
			saveOrUpdate(menu);
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}

		return result;
	}

	@Override
	public List<Menu> findByUserId(long userId) {
		List<Menu> coll = new ArrayList<Menu>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;

			// The administrators can see all menus
			if (user.isMemberOf("admin"))
				return findAll();

			Set<Group> precoll = user.getGroups();
			if (!precoll.isEmpty()) {
				// First of all collect all menus that define it's own policies
				StringBuffer query = new StringBuffer("select distinct(_menu) from Menu _menu  ");
				query.append(" left join _menu.menuGroups as _group ");
				query.append(" where _menu.enabled=1 and _group.groupId in (");
				query.append(precoll.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
				query.append(")");

				coll = (List<Menu>) findByQuery(query.toString(), (Map<String, Object>) null, null);

				// Now collect all menus that references the policies of the
				// previously found menus
				List<Menu> tmp = new ArrayList<Menu>();
				query = new StringBuffer("select _menu from Menu _menu  where _menu.securityRef in (");
				query.append(coll.stream().map(m -> Long.toString(m.getId())).collect(Collectors.joining(",")));
				query.append(")");
				tmp = (List<Menu>) findByQuery(query.toString(), (Map<String, Object>) null, null);

				for (Menu menu : tmp) {
					if (!coll.contains(menu))
						coll.add(menu);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		Collections.sort(coll);
		return coll;
	}

	@Override
	public List<Menu> findByUserId(long userId, long parentId, boolean enabledOnly) {
		return findByUserId(userId, parentId, null, enabledOnly);
	}

	@SuppressWarnings("rawtypes")
	public List<Menu> findByUserId(long userId, long parentId, Integer type, boolean enabledOnly) {
		List<Menu> coll = new ArrayList<Menu>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;
			if (user.isMemberOf("admin"))
				return findByWhere(
						ALIAS_ENTITY + ".id!=" + ALIAS_ENTITY
								+".parentId and " + ALIAS_ENTITY + ".parentId=" + parentId
								+ (type == null ? "" : (" and " + ALIAS_ENTITY + ".type=" + type))
								+ (enabledOnly ? " and " + ALIAS_ENTITY + ".enabled=1" : ""),
						" order by  " + ALIAS_ENTITY + ".position asc, " + ALIAS_ENTITY + ".name asc", null);
			/*
			 * Search for all those menus that defines its own security policies
			 */
			StringBuffer query1 = new StringBuffer();
			Set<Group> precoll = user.getGroups();
			Iterator iter = precoll.iterator();
			if (precoll.isEmpty())
				return coll;

			query1.append("select distinct(" + ALIAS_ENTITY + ") from Menu " + ALIAS_ENTITY + " ");
			query1.append(" left join " + ALIAS_ENTITY + ".menuGroups as _group");
			query1.append(" where _group.groupId in (");

			boolean first = true;
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = (Group) iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parentId);
			query1.append(") and " + ALIAS_ENTITY + ".parentId = :parentId and " + ALIAS_ENTITY + ".id!=" + ALIAS_ENTITY
					+ ".parentId and " + ALIAS_ENTITY + ".enabled=1 ");
			if (type != null)
				query1.append(" and " + ALIAS_ENTITY + ".type = " + type.toString());

			coll = (List<Menu>) findByQuery(query1.toString(), params, null);

			/*
			 * Now search for all other menus that references accessible menus
			 */
			StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Menu " + ALIAS_ENTITY + " where "
					+ ALIAS_ENTITY + ".deleted=0 and " + ALIAS_ENTITY + ".enabled=1 and " + ALIAS_ENTITY
					+ ".parentId = :parentId ");
			query2.append(" and " + ALIAS_ENTITY + ".securityRef in (");
			query2.append("    select distinct(B.id) from Menu B ");
			query2.append(" left join B.menuGroups as _group");
			query2.append(" where _group.groupId in (");

			first = true;
			iter = precoll.iterator();
			while (iter.hasNext()) {
				if (!first)
					query2.append(",");
				Group ug = (Group) iter.next();
				query2.append(Long.toString(ug.getId()));
				first = false;
			}
			query2.append("))");

			List<Menu> coll2 = (List<Menu>) findByQuery(query2.toString(), params, null);
			for (Menu menu : coll2) {
				if (!coll.contains(menu))
					coll.add(menu);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		Collections.sort(coll);
		return coll;
	}

	@Override
	public List<Menu> findChildren(long parentId, Integer max) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parentId);
			return findByWhere(
					ALIAS_ENTITY + ".parentId = :parentId and " + ALIAS_ENTITY + ".id !=" + ALIAS_ENTITY + ".parentId",
					params, null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Menu>();
		}
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List<Menu> findChildren(long parentId, long userId) {
		List<Menu> coll = new ArrayList<Menu>();
		try {
			User user = userDAO.findById(userId);
			if (user.isMemberOf("admin"))
				return findChildren(parentId, null);

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return coll;
			Iterator iter = groups.iterator();

			/*
			 * Search for the menus that define its own policies
			 */
			StringBuffer query1 = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Menu " + ALIAS_ENTITY + "  ");
			query1.append(" left join " + ALIAS_ENTITY + ".menuGroups as _group ");
			query1.append(" where " + ALIAS_ENTITY + ".enabled=1 and _group.groupId in (");

			boolean first = true;
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = (Group) iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}
			query1.append(") and " + ALIAS_ENTITY + ".parentId=" + parentId);
			query1.append(" and not(" + ALIAS_ENTITY + ".id=" + parentId + ")");

			coll = (List<Menu>) findByQuery(query1.toString(), (Map<String, Object>) null, null);

			/*
			 * Now search for all other menus that references accessible menus
			 */
			StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Menu " + ALIAS_ENTITY + " where "
					+ ALIAS_ENTITY + ".deleted=0 and " + ALIAS_ENTITY + ".parentId = :parentId ");
			query2.append(" and " + ALIAS_ENTITY + ".enabled=1 and " + ALIAS_ENTITY + ".securityRef in (");
			query2.append("    select distinct(B.id) from Menu B ");
			query2.append(" left join B.menuGroups as _group");
			query2.append(" where _group.groupId in (");

			first = true;
			iter = groups.iterator();
			while (iter.hasNext()) {
				if (!first)
					query2.append(",");
				Group ug = (Group) iter.next();
				query2.append(Long.toString(ug.getId()));
				first = false;
			}
			query2.append("))");
			query2.append(" and not(" + ALIAS_ENTITY + ".id=" + parentId + ")");

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("parentId", parentId);
			List<Menu> coll2 = (List<Menu>) findByQuery(query2.toString(), params, null);
			for (Menu menu : coll2) {
				if (!coll.contains(menu))
					coll.add(menu);
			}
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			return coll;
		}
		return coll;
	}

	@Override
	public List<Menu> findByParentId(long parentId, boolean enaledOnly) {
		List<Menu> coll = new ArrayList<Menu>();
		List<Menu> temp = findChildren(parentId, null);
		Iterator<Menu> iter = temp.iterator();

		while (iter.hasNext()) {
			Menu menu = iter.next();
			coll.add(menu);

			List<Menu> coll2 = findByParentId(menu.getId(), enaledOnly);

			if (coll2 != null) {
				coll.addAll(coll2);
			}
		}

		return coll;
	}

	private boolean isWriteOrReadEnable(long menuId, long userId, boolean write) {
		boolean result = true;
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return false;
			if (user.isMemberOf("admin"))
				return true;

			long id = menuId;
			Menu menu = findById(menuId);
			if (menu.getSecurityRef() != null)
				id = menu.getSecurityRef().longValue();

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return false;

			StringBuffer query = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Menu " + ALIAS_ENTITY + "  ");
			query.append(" left join " + ALIAS_ENTITY + ".menuGroups as _group ");
			query.append(" where ");
			if (write)
				query.append(" _group.write=1 and ");
			query.append(" _group.groupId in (");
			query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
			query.append(") and " + ALIAS_ENTITY + ".id = :id");

			Map<String, Object> params = new HashMap<String, Object>();
			params.put("id", Long.valueOf(id));

			List<MenuGroup> coll = (List<MenuGroup>) findByQuery(query.toString(), params, null);
			result = coll.size() > 0;
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public boolean isWriteEnable(long menuId, long userId) {
		return isWriteOrReadEnable(menuId, userId, true);
	}

	@Override
	public boolean isReadEnable(long menuId, long userId) {
		return isWriteOrReadEnable(menuId, userId, false);
	}

	@Override
	public String findNameById(long menuId) {
		try {
			Menu menu = this.findById(menuId);
			return menu.getName();
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public List<Long> findMenuIdByUserId(long userId, boolean enabledOnly) {
		return findMenuIdByUserIdAndPermission(userId, Permission.READ, enabledOnly);
	}

	/**
	 * @see com.logicaldoc.core.security.dao.MenuDAO#isMenuWriteable(long, long)
	 */
	public int isMenuWriteable(long menuId, long userId) {
		boolean writePrivilegeBool = isWriteEnable(menuId, userId);
		int writePrivilegeInt = 0;

		if (writePrivilegeBool) {
			writePrivilegeInt = 1;
		}

		return writePrivilegeInt;
	}

	@Override
	public boolean hasWriteAccess(Menu menu, long userId) {
		if (isWriteEnable(menu.getId(), userId) == false) {
			return false;
		}

		List<Menu> children = findByParentId(menu.getId(), false);

		for (Menu subMenu : children) {
			if (!hasWriteAccess(subMenu, userId)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public List<Menu> findByGroupId(long groupId) {
		List<Menu> coll = new ArrayList<Menu>();

		// The administrators can see all menus
		if (groupId == Group.GROUPID_ADMIN)
			return findAll();

		try {
			/*
			 * Search for menus that define its own security policies
			 */
			StringBuffer query = new StringBuffer(
					"select distinct(" + ALIAS_ENTITY + ") from Menu " + ALIAS_ENTITY + "  ");
			query.append(" left join " + ALIAS_ENTITY + ".menuGroups as _group ");
			query.append(" where " + ALIAS_ENTITY + ".deleted=0 and _group.groupId =" + groupId);

			coll = (List<Menu>) findByQuery(query.toString(), (Map<String, Object>) null, null);

			/*
			 * Now search for all other menus that references the previous ones
			 */
			if (!coll.isEmpty()) {
				StringBuffer query2 = new StringBuffer("select " + ALIAS_ENTITY + " from Menu " + ALIAS_ENTITY
						+ " where " + ALIAS_ENTITY + ".deleted=0 ");
				query2.append(" and " + ALIAS_ENTITY + ".securityRef in (");
				boolean first = true;
				for (Menu menu : coll) {
					if (!first)
						query2.append(",");
					query2.append(Long.toString(menu.getId()));
					first = false;
				}
				query2.append(")");
				List<Menu> coll2 = (List<Menu>) findByQuery(query2.toString(), (Map<String, Object>) null, null);
				for (Menu menu : coll2) {
					if (!coll.contains(menu))
						coll.add(menu);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return coll;
	}

	@SuppressWarnings("rawtypes")
	public List<Long> findIdByUserId(long userId, long parentId, Integer type) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;
			if (user.isMemberOf("admin"))
				return findIdsByWhere(ALIAS_ENTITY + ".enabled=1 and " + ALIAS_ENTITY + ".parentId=" + parentId
						+ (type == null ? "" : " and " + ALIAS_ENTITY + ".type=" + type), null, null);

			StringBuffer query1 = new StringBuffer();
			Set<Group> precoll = user.getGroups();
			Iterator iter = precoll.iterator();
			if (!precoll.isEmpty()) {
				query1 = new StringBuffer("select distinct(A.ld_menuid) from ld_menugroup A, ld_menu B "
						+ " where B.ld_enabled=1 and B.ld_deleted=0 and A.ld_menuid=B.ld_id AND B.ld_parentid="
						+ parentId + " AND A.ld_groupid in (");
				boolean first = true;
				while (iter.hasNext()) {
					if (!first)
						query1.append(",");
					Group ug = (Group) iter.next();
					query1.append(Long.toString(ug.getId()));
					first = false;
				}
				query1.append(")");
				if (type != null)
					query1.append(" AND B.ld_type=" + type.toString());

				ids = (List<Long>) queryForList(query1.toString(), Long.class);

				/*
				 * Now find all menus referencing the previously found ones
				 */
				StringBuffer query2 = new StringBuffer(
						"select B.ld_id from ld_menu B where B.ld_deleted=0 and B.ld_enabled=1 ");
				query2.append(" and B.ld_parentid=" + parentId);
				query2.append(" and B.ld_securityref in (");
				query2.append(query1.toString());
				query2.append(")");

				List<Long> menuids2 = (List<Long>) queryForList(query2.toString(), Long.class);
				for (Long menuid : menuids2) {
					if (!ids.contains(menuid))
						ids.add(menuid);
				}

			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return ids;
	}

	@Override
	public List<Menu> findByName(String name) {
		return findByName(null, name, true);
	}

	@Override
	public List<Menu> findByName(Menu parent, String name, boolean caseSensitive) {
		StringBuffer query = null;
		if (caseSensitive)
			query = new StringBuffer(ALIAS_ENTITY + ".name like '" + SqlUtil.doubleQuotes(name) + "' ");
		else
			query = new StringBuffer(
					"lower(" + ALIAS_ENTITY + ".name) like '" + SqlUtil.doubleQuotes(name.toLowerCase()) + "' ");

		if (parent != null)
			query.append(" AND " + ALIAS_ENTITY + ".parentId = " + parent.getId());

		try {
			return findByWhere(query.toString(), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Menu>();
		}
	}

	@Override
	public String computePathExtended(long menuId) {
		Menu menu = null;
		try {
			menu = findById(menuId);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		if (menu == null)
			return null;
		String path = menuId != 1 ? menu.getName() : "";
		while (menu != null && menu.getId() != menu.getParentId() && menu.getId() != 1) {
			try {
				menu = findById(menu.getParentId());
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
			if (menu != null)
				path = (menu.getId() != 1 ? menu.getName() : "") + "/" + path;
		}
		if (!path.startsWith("/"))
			path = "/" + path;
		return path;

	}

	@Override
	public List<Menu> findByNameAndParentId(String name, long parentId) {
		try {
			return findByWhere(ALIAS_ENTITY + ".parentId = " + parentId + " and " + ALIAS_ENTITY + ".name like '"
					+ SqlUtil.doubleQuotes(name) + "'", null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Menu>();
		}
	}

	@Override
	public List<Menu> findParents(long menuId) {
		List<Menu> coll = new ArrayList<Menu>();
		try {
			Menu menu = findById(menuId);
			while (menu != null && menu.getId() != 1 && menu.getId() != menu.getParentId()) {
				menu = findById(menu.getParentId());
				if (menu != null)
					coll.add(0, menu);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return coll;
	}

	@Override
	public void restore(long menuId, boolean parents) throws PersistenceException {
		bulkUpdate("set ld_deleted=0 where ld_id=" + menuId, (Map<String, Object>) null);

		// Restore parents
		if (parents) {
			String query = "select ld_parentid from ld_menu where ld_id =" + menuId;

			try {
				List<Long> menus = (List<Long>) super.queryForList(query, Long.class);
				for (Long id : menus) {
					if (id.longValue() != menuId)
						restore(id, parents);
				}
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public List<Long> findMenuIdByUserIdAndPermission(long userId, Permission permission, boolean enabledOnly) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			// The administrators have all permissions on all menus
			if (user.isMemberOf("admin")) {
				return findAllIds();
			}

			Set<Group> precoll = user.getGroups();
			Iterator<Group> iter = precoll.iterator();

			if (!precoll.isEmpty()) {
				/*
				 * Check menus that specify its own permissions
				 */
				StringBuffer query1 = new StringBuffer("select distinct(A.ld_menuid) from ld_menugroup A, ld_menu B "
						+ " where A.ld_menuid=B.ld_id and B.ld_deleted=0 ");
				if (enabledOnly)
					query1.append(" and B.ld_enabled=1 ");
				if (permission != Permission.READ)
					query1.append(" and A.ld_" + permission.getName() + "=1 ");
				query1.append(" and A.ld_groupid in (");
				boolean first = true;
				while (iter.hasNext()) {
					if (!first)
						query1.append(",");
					Group ug = (Group) iter.next();
					query1.append(Long.toString(ug.getId()));
					first = false;
				}
				query1.append(")");

				ids = (List<Long>) queryForList(query1.toString(), Long.class);

				/*
				 * Now search for those menus that references the previously
				 * found ones
				 */
				StringBuffer query2 = new StringBuffer("select B.ld_id from ld_menu B where B.ld_deleted=0 ");
				query2.append(" and B.ld_securityref in (" + query1.toString() + ")");

				List<Long> mrefs = (List<Long>) queryForList(query2.toString(), Long.class);
				for (Long menuId : mrefs) {
					if (!ids.contains(menuId))
						ids.add(menuId);
				}

			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return ids;
	}

	@Override
	public void deleteAll(Collection<Menu> menus) throws PersistenceException {
		for (Menu menu : menus) {
			try {
				delete(menu.getId());
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public boolean delete(long menuId, int code) throws PersistenceException {
		boolean result = true;
		Menu menu = (Menu) findById(menuId);
		menu.setDeleted(code);
		result = store(menu);
		return result;
	}

	@Override
	public boolean applyRightToTree(long id) {
		boolean result = true;
		try {
			Menu parent = findById(id);
			Long securityRef = id;
			if (parent.getSecurityRef() != null)
				securityRef = parent.getSecurityRef();

			// Iterate over all children setting the security reference
			List<Menu> children = findChildren(id, null);
			for (Menu menu : children) {
				if (!securityRef.equals(menu.getSecurityRef())) {
					menu.setSecurityRef(securityRef);
					menu.getMenuGroups().clear();
					store(menu);
				}
				applyRightToTree(menu.getId());
			}
		} catch (Throwable e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List<Long> findIdByUserId(long userId, long parentId) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;
			if (user.isMemberOf("admin"))
				return findIdsByWhere(ALIAS_ENTITY + ".parentId=" + parentId, null, null);

			StringBuffer query1 = new StringBuffer();
			Set<Group> precoll = user.getGroups();
			Iterator iter = precoll.iterator();
			if (!precoll.isEmpty()) {
				query1 = new StringBuffer("select distinct(A.ld_menuid) from ld_menugroup A, ld_menu B "
						+ " where B.ld_deleted=0 and A.ld_menuid=B.ld_id AND B.ld_parentid=" + parentId
						+ " AND A.ld_groupid in (");
				boolean first = true;
				while (iter.hasNext()) {
					if (!first)
						query1.append(",");
					Group ug = (Group) iter.next();
					query1.append(Long.toString(ug.getId()));
					first = false;
				}
				query1.append(")");

				ids = (List<Long>) queryForList(query1.toString(), Long.class);

				/*
				 * Now find all menus referencing the previously found ones
				 */
				StringBuffer query2 = new StringBuffer("select B.ld_id from ld_menu B where B.ld_deleted=0 ");
				query2.append(" and B.ld_parentid=" + parentId);
				query2.append(" and B.ld_securityref in (");
				query2.append(query1.toString());
				query2.append(")");

				List<Long> menuids2 = (List<Long>) queryForList(query2.toString(), Long.class);
				for (Long menuid : menuids2) {
					if (!ids.contains(menuid))
						ids.add(menuid);
				}

			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return ids;
	}

	@Override
	public Menu createPath(long parentId, long tenantId, int type, String path, boolean inheritSecurity)
			throws PersistenceException {
		if (!checkStoringAspect())
			return null;

		StringTokenizer st = new StringTokenizer(path, "/", false);

		Menu menu = findById(parentId);
		while (st.hasMoreTokens()) {
			initialize(menu);
			String name = st.nextToken();

			List<Menu> childs = findByName(menu, name, false);
			if (childs.isEmpty()) {
				Menu newMenu = new Menu();
				newMenu.setName(name);
				newMenu.setParentId(menu.getId());
				newMenu.setTenantId(tenantId);
				newMenu.setType(type);

				if (inheritSecurity) {
					if (menu.getSecurityRef() != null)
						newMenu.setSecurityRef(menu.getSecurityRef());
					else
						newMenu.setSecurityRef(menu.getId());
				}

				store(newMenu);
				flush();
				menu = newMenu;
				log.info("created menu {}", computePathExtended(newMenu.getId()));
			} else {
				menu = childs.iterator().next();
				initialize(menu);
			}
		}

		return menu;
	}
}