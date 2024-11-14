package com.logicaldoc.core.security.menu;

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

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.history.HibernatePersistentObjectDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.AccessControlUtil;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>MenuDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@SuppressWarnings("unchecked")
public class HibernateMenuDAO extends HibernatePersistentObjectDAO<Menu> implements MenuDAO {

	private static final String SELECT_DISTINCT_A_LD_MENUID_FROM_LD_MENUGROUP_A_LD_MENU_B = "select distinct(A.ld_menuid) from ld_menu_acl A, ld_menu B ";

	private static final String ACL_AS_GROUP = ".accessControlList as _group ";

	private static final String WHERE = " where ";

	private static final String ENABLED_1_AND = ".enabled=1 and ";

	private static final String CLOSE_BRACKET_AND = ") and ";

	private static final String WHERE_GROUP_GROUP_ID_IN = " where _group.groupId in (";

	private static final String SELECT_DISTINCT = "select distinct(";

	private static final String CLOSE_BRACKET_FROM_MENU = ") from Menu ";

	private static final String DOT_PARENT_ID = ".parentId=";

	private static final String PARENT_ID = "parentId";

	private static final String LEFT_JOIN = " left join ";

	private static final String AND = " and ";

	@Resource(name = "UserDAO")
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
	public void store(Menu menu) throws PersistenceException {
		AccessControlUtil.removeForbiddenPermissionsForGuests(menu);
		super.store(menu);
	}

	@Override
	public List<Menu> findByUserId(long userId) {
		List<Menu> coll = new ArrayList<>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;

			// The administrators can see all menus
			if (user.isAdmin())
				return findAll();

			Set<Group> groups = user.getGroups();
			if (!groups.isEmpty()) {
				// First of all collect all menus that define it's own policies
				StringBuilder query = new StringBuilder("select distinct(_menu) from Menu _menu  ");
				query.append(" left join _menu.accessControlList as _group ");
				query.append(" where _menu.enabled=1 and _group.groupId in (");
				query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
				query.append(")");

				coll = findByQuery(query.toString(), (Map<String, Object>) null, null);
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

	public List<Menu> findByUserId(long userId, long parentId, Integer type, boolean enabledOnly) {
		List<Menu> coll = new ArrayList<>();

		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return coll;
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return findByWhere(
						ENTITY + ".id!=" + ENTITY + ".parentId and " + ENTITY + DOT_PARENT_ID + parentId
								+ (type == null ? "" : (AND + ENTITY + ".type=" + type))
								+ (enabledOnly ? AND + ENTITY + ".enabled=1" : ""),
						" order by  " + ENTITY + ".position asc, " + ENTITY + ".name asc", null);
			/*
			 * Search for all those menus that defines its own security policies
			 */
			if (user.getGroups().isEmpty())
				return coll;
			StringBuilder query1 = new StringBuilder();
			query1.append(SELECT_DISTINCT + ENTITY + CLOSE_BRACKET_FROM_MENU + ENTITY + " ");
			query1.append(LEFT_JOIN + ENTITY + ".accessControlList as _group");
			query1.append(WHERE_GROUP_GROUP_ID_IN);
			query1.append(
					user.getGroups().stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));

			Map<String, Object> params = new HashMap<>();
			params.put(PARENT_ID, parentId);
			query1.append(CLOSE_BRACKET_AND + ENTITY + ".parentId = :parentId and " + ENTITY + ".id!=" + ENTITY
					+ ".parentId and " + ENTITY + ".enabled=1 ");
			if (type != null)
				query1.append(AND + ENTITY + ".type = " + type.toString());

			coll = findByQuery(query1.toString(), params, null);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		Collections.sort(coll);
		return coll;
	}

	@Override
	public List<Menu> findChildren(long parentId, Integer max) {
		try {
			Map<String, Object> params = new HashMap<>();
			params.put(PARENT_ID, parentId);
			return findByWhere(ENTITY + ".parentId = :parentId and " + ENTITY + ".id !=" + ENTITY + ".parentId", params,
					null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List<Menu> findChildren(long parentId, long userId) {
		List<Menu> coll = new ArrayList<>();
		try {
			User user = userDAO.findById(userId);
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return findChildren(parentId, null);

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return coll;
			Iterator iter = groups.iterator();

			/*
			 * Search for the menus that define its own policies
			 */
			StringBuilder query1 = new StringBuilder(
					SELECT_DISTINCT + ENTITY + CLOSE_BRACKET_FROM_MENU + ENTITY + "  ");
			query1.append(LEFT_JOIN + ENTITY + ACL_AS_GROUP);
			query1.append(WHERE + ENTITY + ".enabled=1 and _group.groupId in (");

			boolean first = true;
			while (iter.hasNext()) {
				if (!first)
					query1.append(",");
				Group ug = (Group) iter.next();
				query1.append(Long.toString(ug.getId()));
				first = false;
			}
			query1.append(CLOSE_BRACKET_AND + ENTITY + DOT_PARENT_ID + parentId);
			query1.append(" and not(" + ENTITY + ".id=" + parentId + ")");

			coll = findByQuery(query1.toString(), (Map<String, Object>) null, null);
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			return coll;
		}
		return coll;
	}

	@Override
	public List<Menu> findByParentId(long parentId, boolean enaledOnly) {
		List<Menu> coll = new ArrayList<>();
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
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return true;

			Set<Group> groups = user.getGroups();
			if (groups.isEmpty())
				return false;

			StringBuilder query = new StringBuilder(SELECT_DISTINCT + ENTITY + CLOSE_BRACKET_FROM_MENU + ENTITY + "  ");
			query.append(LEFT_JOIN + ENTITY + ACL_AS_GROUP);
			query.append(WHERE);
			query.append(" _group.read=1 and ");
			if (write)
				query.append(" _group.write=1 and ");
			query.append(" _group.groupId in (");
			query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
			query.append(CLOSE_BRACKET_AND + ENTITY + ".id = :id");

			Map<String, Object> params = new HashMap<>();
			params.put("id", Long.valueOf(menuId));

			List<AccessControlEntry> coll = findByQuery(query.toString(), params, null);
			result = CollectionUtils.isNotEmpty(coll);
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
	 * @see com.logicaldoc.core.security.menu.MenuDAO#isMenuWriteable(long,
	 *      long)
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
		if (!isWriteEnable(menu.getId(), userId)) {
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
	public List<Menu> findByGroupId(long groupId) throws PersistenceException {
		// The administrators can see all menus
		if (groupId == Group.GROUPID_ADMIN)
			return findAll();

		/*
		 * Search for menus that define its own security policies
		 */
		StringBuilder query = new StringBuilder(SELECT_DISTINCT + ENTITY + CLOSE_BRACKET_FROM_MENU + ENTITY + "  ");
		query.append(LEFT_JOIN + ENTITY + ACL_AS_GROUP);
		query.append(WHERE + ENTITY + ".deleted=0 and _group.read=1 and _group.groupId =" + groupId);

		return findByQuery(query.toString(), (Map<String, Object>) null, null);
	}

	public List<Long> findIdByUserId(long userId, long parentId, Integer type) {
		List<Long> ids = new ArrayList<>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return findIdsByWhere(ENTITY + ENABLED_1_AND + ENTITY + DOT_PARENT_ID + parentId
						+ (type == null ? "" : AND + ENTITY + ".type=" + type), null, null);

			if (!user.getGroups().isEmpty()) {
				StringBuilder query1 = new StringBuilder(SELECT_DISTINCT_A_LD_MENUID_FROM_LD_MENUGROUP_A_LD_MENU_B
						+ " where B.ld_enabled=1 and B.ld_deleted=0 and A.ld_menuid=B.ld_id and A.ld_read=1 AND B.ld_parentid="
						+ parentId + " AND A.ld_groupid in (");
				query1.append(
						user.getGroups().stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
				query1.append(")");
				if (type != null)
					query1.append(" AND B.ld_type=" + type.toString());

				ids = queryForList(query1.toString(), Long.class);
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
		StringBuilder query = null;
		if (caseSensitive)
			query = new StringBuilder(ENTITY + ".name like '" + SqlUtil.doubleQuotes(name) + "' ");
		else
			query = new StringBuilder(
					"lower(" + ENTITY + ".name) like '" + SqlUtil.doubleQuotes(name.toLowerCase()) + "' ");

		if (parent != null)
			query.append(" AND " + ENTITY + ".parentId = " + parent.getId());

		try {
			return findByWhere(query.toString(), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
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
			if (menu != null) {
				StringBuilder sb = new StringBuilder(menu.getId() != 1 ? menu.getName() : "");
				sb.append("/");
				sb.append(path);
				path = sb.toString();
			}
		}
		if (!path.startsWith("/"))
			path = "/" + path;
		return path;

	}

	@Override
	public List<Menu> findByNameAndParentId(String name, long parentId) {
		try {
			return findByWhere(ENTITY + ".parentId = " + parentId + AND + ENTITY + ".name like '"
					+ SqlUtil.doubleQuotes(name) + "'", null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<Menu> findParents(long menuId) {
		List<Menu> coll = new ArrayList<>();
		try {
			Menu menu = findById(menuId);
			while (menu != null && menu.getId() != 1 && menu.getId() != menu.getParentId()) {
				menu = findById(menu.getParentId());
				if (menu != null)
					coll.add(0, menu);
			}
		} catch (Exception e) {
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
				List<Long> menus = super.queryForList(query, Long.class);
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
		List<Long> ids = new ArrayList<>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			// The administrators have all permissions on all menus
			if (user.isMemberOf(Group.GROUP_ADMIN)) {
				return findAllIds();
			}

			if (!user.getGroups().isEmpty()) {
				/*
				 * Check menus that specify its own permissions
				 */
				StringBuilder query1 = new StringBuilder(SELECT_DISTINCT_A_LD_MENUID_FROM_LD_MENUGROUP_A_LD_MENU_B
						+ " where A.ld_menuid=B.ld_id and B.ld_deleted=0 ");
				if (enabledOnly)
					query1.append(" and B.ld_enabled=1 ");

				query1.append(" and A.ld_" + permission.getName() + "=1 ");
				query1.append(" and A.ld_groupid in (");
				query1.append(
						user.getGroups().stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
				query1.append(")");

				ids = queryForList(query1.toString(), Long.class);
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
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		}
	}

	@Override
	public void delete(long menuId, int code) throws PersistenceException {
		Menu menu = findById(menuId);
		menu.setDeleted(code);
		store(menu);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List<Long> findIdByUserId(long userId, long parentId) {
		List<Long> ids = new ArrayList<>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;
			if (user.isMemberOf(Group.GROUP_ADMIN))
				return findIdsByWhere(ENTITY + DOT_PARENT_ID + parentId, null, null);

			Set<Group> precoll = user.getGroups();
			Iterator iter = precoll.iterator();
			if (!precoll.isEmpty()) {
				StringBuilder query1 = new StringBuilder(SELECT_DISTINCT_A_LD_MENUID_FROM_LD_MENUGROUP_A_LD_MENU_B
						+ " where B.ld_deleted=0 and A.ld_menuid=B.ld_id AND B.ld_parentid=" + parentId
						+ " and A.ld_read=1 AND A.ld_groupid in (");
				boolean first = true;
				while (iter.hasNext()) {
					if (!first)
						query1.append(",");
					Group ug = (Group) iter.next();
					query1.append(Long.toString(ug.getId()));
					first = false;
				}
				query1.append(")");

				ids = queryForList(query1.toString(), Long.class);
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
				menu = createNewMenu(name, tenantId, type, menu);
			} else {
				menu = childs.iterator().next();
				initialize(menu);
			}
		}

		return menu;
	}

	private Menu createNewMenu(String name, long tenantId, int type, Menu parentMenu) throws PersistenceException {
		Menu newMenu = new Menu();
		newMenu.setName(name);
		newMenu.setParentId(parentMenu.getId());
		newMenu.setTenantId(tenantId);
		newMenu.setType(type);

		store(newMenu);
		flush();
		parentMenu = newMenu;
		if (log.isInfoEnabled())
			log.info("created menu {}", computePathExtended(newMenu.getId()));
		return parentMenu;
	}
}