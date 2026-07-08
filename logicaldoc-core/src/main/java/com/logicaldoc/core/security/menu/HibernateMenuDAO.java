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

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.spring.Context;

import jakarta.annotation.Resource;
import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of {@link MenuDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Repository("menuDAO")
@Transactional
public class HibernateMenuDAO extends HibernatePersistentObjectDAO<Menu> implements MenuDAO {

    private static final String PARENT_ID = "parentId";

    @Resource(name = "userDAO")
    protected UserDAO userDAO;

    public HibernateMenuDAO() {
        super(Menu.class);
        super.log = LoggerFactory.getLogger(HibernateMenuDAO.class);
    }

    @Override
    public void store(Menu menu) throws PersistenceException {
        removeForbiddenPermissionsForGuests(menu);
        super.store(menu);
    }

    private void removeForbiddenPermissionsForGuests(Menu menu) throws PersistenceException {
        // Remove the forbidden permissions for the guests
        GroupDAO gDao = Context.get(GroupDAO.class);
        for (AccessControlEntry ace : menu.getAccessControlList()) {
            Group group = gDao.findById(ace.getGroupId());
            if (group != null && group.isGuest()) {
                ace.setWrite(false);
            }
        }
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
                query.append(" where _menu.enabled = true and _group.groupId in (");
                query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
                query.append(")");

                coll = findByObjectQuery(query.toString(), (Map<String, Object>) null, null);
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
                        "_entity.id != _entity.parentId and _entity.parentId = %d %s %s".formatted(parentId,
                                type != null ? ("and _entity.type = %d".formatted(type)) : "",
                                enabledOnly ? "and _entity.enabled = true " : ""),
                        "_entity.position asc, _entity.name asc", null);
            /*
             * Search for all those menus that defines its own security policies
             */
            if (user.getGroups().isEmpty())
                return coll;
            StringBuilder query1 = new StringBuilder("""
                                                        select distinct(_entity)
                                                          from Menu _entity
                                                     left join _entity.accessControlList as _group
                                                         where _group.groupId in (
                                                        """);
            query1.append(
                    user.getGroups().stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
            query1.append("""
                          ) and _entity.parentId = :parentId
                            and _entity.id != _entity.parentId
                            and _entity.enabled = true
                          """);
            if (type != null)
                query1.append(" and _entity.type = %s".formatted(type.toString()));

            coll = findByObjectQuery(query1.toString(), Map.of(PARENT_ID, parentId), null);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }

        Collections.sort(coll);
        return coll;
    }

    @Override
    public List<Menu> findChildren(long parentId, Integer max) {
        try {
            return findByWhere("_entity.parentId = :parentId and _entity.id != _entity.parentId",
                    Map.of(PARENT_ID, parentId), null, max);
        } catch (PersistenceException e) {
            log.error(e.getMessage(), e);
            return new ArrayList<>();
        }
    }

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

            /*
             * Search for the menus that define its own policies
             */
            StringBuilder query1 = new StringBuilder("""
                                                        select distinct(_entity)
                                                          from Menu _entity
                                                     left join _entity.accessControlList as _group
                                                         where _entity.enabled = true
                                                           and _group.groupId in (
                                                        """);
            query1.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
            query1.append("""
                          ) and _entity.parentId = :parentId
                           and not _entity.id = :parentId
                          """);

            coll = findByObjectQuery(query1.toString(), Map.of(PARENT_ID, parentId), null);
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
        try {
            User user = userDAO.findById(userId);
            if (user == null)
                return false;
            if (user.isMemberOf(Group.GROUP_ADMIN))
                return true;

            Set<Group> groups = user.getGroups();
            if (groups.isEmpty())
                return false;

            StringBuilder query = new StringBuilder("""
                                                       select distinct(_entity)
                                                         from Menu _entity
                                                    left join _entity.accessControlList as _group
                                                        where _group.read = true
                                                          and
                                                        """);
            if (write)
                query.append(" _group.write = true and ");
            query.append(" _group.groupId in (");
            query.append(groups.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
            query.append(") and _entity.id = :id");

            return CollectionUtils
                    .isNotEmpty(findByObjectQuery(query.toString(), Map.of("id", Long.valueOf(menuId)), null));
        } catch (Exception e) {
            if (log.isErrorEnabled())
                log.error(e.getMessage(), e);
            return false;
        }
    }

    @Override
    public boolean isWriteAllowed(long menuId, long userId) {
        return isWriteOrReadEnable(menuId, userId, true);
    }

    @Override
    public boolean isReadAllowed(long menuId, long userId) {
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

    @Override
    public List<Menu> findByGroupId(long groupId) throws PersistenceException {
        // The administrators can see all menus
        if (groupId == Group.GROUPID_ADMIN)
            return findAll();

        /*
         * Search for menus that define its own security policies
         */
        return findByObjectQuery("""
                                    select distinct(_entity)
                                      from Menu _entity
                                 left join _entity.accessControlList as _group
                                     where _entity.deleted = 0
                                       and _group.read = true
                                       and _group.groupId = %d
                                       """.formatted(groupId), (Map<String, Object>) null, null);
    }

    public List<Long> findIdByUserId(long userId, long parentId, Integer type) {
        List<Long> ids = new ArrayList<>();
        try {
            User user = userDAO.findById(userId);
            if (user == null)
                return ids;
            if (user.isMemberOf(Group.GROUP_ADMIN))
                return findIdsByWhere("_entity.enabled = true and _entity.parentId = %d %s".formatted(parentId,
                        type == null ? "" : " and _entity.type = %d".formatted(type)), null, null);

            if (!user.getGroups().isEmpty()) {
                StringBuilder query1 = new StringBuilder("""
                                                         select distinct(A.ld_menuid)
                                                           from ld_menu_acl A, ld_menu B
                                                          where B.ld_enabled = 1
                                                            and B.ld_deleted = 0
                                                            and A.ld_menuid = B.ld_id
                                                            and A.ld_read = 1
                                                            and B.ld_parentid = %d"
                                                            and A.ld_groupid in (
                                                         """.formatted(parentId));
                query1.append(
                        user.getGroups().stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")));
                query1.append(")");
                if (type != null)
                    query1.append(" AND B.ld_type = %s".formatted(type));

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
        Map<String, Object> params = new HashMap<>();
        params.put("name", caseSensitive ? name : name.toLowerCase());
        StringBuilder query = new StringBuilder();

        if (caseSensitive)
            query.append("_entity.name like :name");
        else
            query.append("lower(_entity.name) like :name");

        if (parent != null) {
            params.put(PARENT_ID, parent.getId());
            query.append(" and _entity.parentId = :parentId");
        }

        try {
            return findByWhere(query.toString(), params, null, null);
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
            path = "/%s".formatted(path);
        return path;

    }

    @Override
    public List<Menu> findByNameAndParentId(String name, long parentId) {
        try {
            return findByWhere("_entity.parentId = :parentId and _entity.name like :name",
                    Map.of(PARENT_ID, parentId, "name", name), null, null);
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
        jdbcUpdate("update ld_menu set ld_deleted = 0 where ld_id = %d".formatted(menuId));

        // Restore parents
        if (parents) {
            try {
                List<Long> menus = super.queryForList(
                        "select ld_parentid from ld_menu where ld_id = %d".formatted(menuId), Long.class);
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
                StringBuilder query = new StringBuilder("""
                                                        select distinct(A.ld_menuid)
                                                          from ld_menu_acl A, ld_menu B
                                                         where A.ld_menuid = B.ld_id
                                                           and B.ld_deleted = 0
                                                        """);
                if (enabledOnly)
                    query.append(" and B.ld_enabled = 1 ");

                query.append(" and A.ld_%s = 1 ".formatted(permission.name().toLowerCase()))
                        .append(" and A.ld_groupid in (").append(user.getGroups().stream()
                                .map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")))
                        .append(")");

                ids = queryForList(query.toString(), Long.class);
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

    @Override
    public List<Long> findIdByUserId(long userId, long parentId) {
        List<Long> ids = new ArrayList<>();
        try {
            User user = userDAO.findById(userId);
            if (user == null)
                return ids;
            if (user.isMemberOf(Group.GROUP_ADMIN))
                return findIdsByWhere("_entity.parentId = %d".formatted(parentId), null, null);

            Set<Group> precoll = user.getGroups();
            if (!precoll.isEmpty()) {
                StringBuilder query = new StringBuilder("""
                                                        select distinct(A.ld_menuid)
                                                          from ld_menu_acl A, ld_menu B
                                                         where B.ld_deleted = 0
                                                           and A.ld_menuid = B.ld_id
                                                           and B.ld_parentid = %d
                                                           and A.ld_read = 1
                                                           and A.ld_groupid in (
                                                        """.formatted(parentId));
                query.append(precoll.stream().map(g -> Long.toString(g.getId())).collect(Collectors.joining(",")))
                        .append(")");

                ids = queryForList(query.toString(), Long.class);
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