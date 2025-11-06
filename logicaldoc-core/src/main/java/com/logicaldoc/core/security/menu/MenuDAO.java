package com.logicaldoc.core.security.menu;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.util.spring.Context;

/**
 * Instances of this class is a DAO-service for menu objects.
 * 
 * @author Michael Scholz
 * @version 1.0
 */
public interface MenuDAO extends PersistentObjectDAO<Menu> {
	
	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static MenuDAO get() {
		return Context.get(MenuDAO.class);
	}
	
	/**
	 * Finds all menus by menu text.
	 * 
	 * @param name name of the menu
	 * 
	 * @return List of menus with given menu text.
	 */
	public List<Menu> findByName(String name);

	/**
	 * Finds all menus by menu text, contained in the parent menu
	 * 
	 * @param parent The parent menu(optional)
	 * @param name the name to search
	 * @param caseSensitive if we have to consider the search case sensitive
	 * 
	 * @return List of menus with given menu name.
	 */
	public List<Menu> findByName(Menu parent, String name, boolean caseSensitive);

	/**
	 * Finds authorized menus for a user.
	 * 
	 * @param userId ID of the user.
	 * @return List of found menus.
	 */
	public List<Menu> findByUserId(long userId);

	/**
	 * Finds all menus ids with a specific permission enabled on the specified
	 * user
	 * 
	 * @param userId The user identifier
	 * @param permission The permission to check
	 * @param enabledOnly if the menus must also be enabled
	 * 
	 * @return list of folder IDs
	 */
	public List<Long> findMenuIdByUserIdAndPermission(long userId, Permission permission, boolean enabledOnly);

	/**
	 * Finds direct children of a menu
	 * 
	 * @param userId identifier of the user
	 * @param parentId MenuId of the menu which children are wanted
	 * @param enabledOnly if the menus must also be enabled
	 * 
	 * @return List of found menus sorted by name
	 */
	public List<Menu> findByUserId(long userId, long parentId, boolean enabledOnly);

	/**
	 * Finds all children(direct and indirect) by parentId
	 * 
	 * @param parentId identifier of the parent menu
	 * @param enabledOnly if the menus must also be enabled
	 * 
	 * @return list of children menus
	 */
	public List<Menu> findByParentId(long parentId, boolean enabledOnly);

	/**
	 * Finds direct children of a menu.
	 * 
	 * @param parentId MenuId of the menu which children are wanted
	 * @param max Optional, maximum number of children
	 * 
	 * @return List of found menus
	 */
	public List<Menu> findChildren(long parentId, Integer max);

	/**
	 * Finds direct children of a menu accessible by the given user.
	 * 
	 * @param parentId MenuId of the menu which children are wanted
	 * @param userId Identifier of the user that must have read access
	 * 
	 * @return List of found menus.
	 */
	public List<Menu> findChildren(long parentId, long userId);

	/**
	 * This method is looking up for writing rights for a menu and an user
	 * 
	 * @param id ID of the menu
	 * @param userId ID of the user
	 * 
	 * @return id the user has write permission
	 */
	public boolean isWriteAllowed(long id, long userId);

	/**
	 * This method is looking up for read rights for a menu and an user
	 * 
	 * @param id ID of the menu
	 * @param userId ID of the user
	 * 
	 * @return if the user can access the menu
	 */
	public boolean isReadAllowed(long id, long userId);

	/**
	 * This method selects only the menu text from a menu
	 * 
	 * @param id Id of the menu
	 * 
	 * @return Selected menu text
	 */
	public String findNameById(long id);

	/**
	 * This method selects only the menuId from the menus for which a user is
	 * authorized
	 * 
	 * @param userId ID of the user
	 * @param enabledOnly if the menus must also be enabled
	 * 
	 * @return List of selected menuId's
	 */
	public List<Long> findMenuIdByUserId(long userId, boolean enabledOnly);

	/**
	 * This method selects only the menuId from the menus for which a user is
	 * authorized. Only menus direct child of the specified parent are returned.
	 * 
	 * @param userId ID of the user.
	 * @param parentId Parent menu
	 * @return List of selected menuId's.
	 */
	public List<Long> findIdByUserId(long userId, long parentId);

	/**
	 * Finds all menus accessible by the passed group
	 * 
	 * @param groupId The group id
	 * 
	 * @return The List of menus
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<Menu> findByGroupId(long groupId) throws PersistenceException;

	/**
	 * Returns a List of menus being a parent of the given menu. The list is
	 * ordered starting from the root of menus.
	 * 
	 * @param id identifier of the group
	 * 
	 * @return hierarchy of parents
	 */
	public List<Menu> findParents(long id);

	/**
	 * Restores a previously deleted menu
	 * 
	 * @param id The menu identifier
	 * @param parents true if parents must be restored also
	 * 
	 * @throws PersistenceException is happened a database error
	 */
	public void restore(long id, boolean parents) throws PersistenceException;

	/**
	 * Finds that folder that lies under a specific parent (given by the id) an
	 * with a given text(like operator is used)
	 * 
	 * @param name name of the menu
	 * @param parentId identifier of the parent menu
	 * 
	 * @return list of manues
	 */
	public List<Menu> findByNameAndParentId(String name, long parentId);

	/**
	 * Dynamically computes the path extended for the specified menu. The path
	 * extended is a human readable path in the form: /menu1/menu2/menu3
	 * 
	 * @param id identifier of the menu
	 * 
	 * @return full path of the menu
	 */
	public String computePathExtended(long id);

	/**
	 * Creates the menu for the specified path. All unexisting nodes specified
	 * in the path will be created.
	 * 
	 * @param parentId Identifier of the parent menu
	 * @param tenantId Identifier of the tenant that will own the new menu
	 * @param type The type of the created menus
	 * @param path The folder path(for example /dog/cat/mouse)
	 * @param inheritSecurity If true the new menus will 'point' to the parent
	 *        for the security policies.
	 * 
	 * @return The created folder
	 * 
	 * @throws PersistenceException error at data layer
	 */
	public Menu createPath(long parentId, long tenantId, int type, String path, boolean inheritSecurity)
			throws PersistenceException;
}