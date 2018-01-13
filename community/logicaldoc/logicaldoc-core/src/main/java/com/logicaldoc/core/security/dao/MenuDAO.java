package com.logicaldoc.core.security.dao;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Permission;

/**
 * Instances of this class is a DAO-service for menu objects.
 * 
 * @author Michael Scholz
 * @version 1.0
 */
public interface MenuDAO extends PersistentObjectDAO<Menu> {

	/**
	 * Finds all menus by menu text.
	 * 
	 * @param text
	 * @return List of menus with given menu text.
	 */
	public List<Menu> findByName(String text);

	/**
	 * Finds all menus by menu text, contained in the parent menu
	 * 
	 * @param parent The parent menu(optional)
	 * @param text The menutext to search for
	 * @param caseSensitive
	 * @return List of menus with given menu text.
	 */
	public List<Menu> findByName(Menu parent, String text, boolean caseSensitive);

	/**
	 * Finds authorized menus for a user.
	 * 
	 * @param userId ID of the user.
	 * @return List of found menus.
	 */
	public List<Menu> findByUserId(long userId);

	/**
	 * Finds all menus ids with a specific permission enabled on the specifies
	 * user
	 * 
	 * @param userId The user identifier
	 * @param permission The permission to check
	 * @return
	 */
	public List<Long> findMenuIdByUserIdAndPermission(long userId, Permission permission);

	/**
	 * Finds direct children of a menu.
	 * 
	 * @param parentId MenuId of the menu which children are wanted.
	 * @return List of found menus sorted by text
	 */
	public List<Menu> findByUserId(long userId, long parentId);

	/**
	 * Finds all children(direct and indirect) by parentId
	 * 
	 * @param parentId
	 * @return
	 */
	public List<Menu> findByParentId(long parentId);

	/**
	 * Finds direct children of a menu.
	 * 
	 * @param parentId MenuId of the menu which children are wanted
	 * @param max Optional, maximum number of children
	 * @return List of found menus
	 */
	public List<Menu> findChildren(long parentId, Integer max);

	/**
	 * Finds direct children of a menu accessible by the given user.
	 * 
	 * @param parentId MenuId of the menu which children are wanted
	 * @param userId Identifier of the user that mush have read access
	 * 
	 * @return List of found menus.
	 */
	public List<Menu> findChildren(long parentId, long userId);

	/**
	 * This method is looking up for writing rights for a menu and an user.
	 * 
	 * @param id ID of the menu.
	 * @param userId ID of the user.
	 */
	public boolean isWriteEnable(long id, long userId);

	public boolean isReadEnable(long id, long userId);

	/**
	 * This method selects only the menu text from a menu.
	 * 
	 * @param id Id of the menu.
	 * @return Selected menu text.
	 */
	public String findNameById(long id);

	/**
	 * This method selects only the menuId from the menus for which a user is
	 * authorized.
	 * 
	 * @param userId ID of the user.
	 * @return List of selected menuId's.
	 */
	public List<Long> findMenuIdByUserId(long userId);

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
	 * Returns if a menu is writeable for a user
	 * 
	 * @param id check this menu
	 * @param userId privileges for this should be checked
	 * @return a 0 if false, a 1 if true
	 */
	public int isMenuWriteable(long id, long userId);

	/**
	 * Checks that the user has access to the menu and all its sub-items
	 */
	public boolean hasWriteAccess(Menu menu, long userId);

	/**
	 * Finds all menus accessible by the passed group
	 * 
	 * @param groupId The group id
	 * @return The List of menus
	 */
	public List<Menu> findByGroupId(long groupId);

	/**
	 * Returns a List of menus being a parent of the given menu. The list is
	 * ordered starting from the root of menus.
	 * 
	 * @param id
	 */
	public List<Menu> findParents(long id);

	/**
	 * Restores a previously deleted menu
	 * 
	 * @param id The menu identifier
	 * @param parents true if parents must be restored also
	 */
	public void restore(long id, boolean parents);

	/**
	 * Finds that folder that lies under a specific parent (given by the id) an
	 * with a given text(like operator is used)
	 * 
	 * @param text
	 * @param parentId
	 * @return
	 */
	public List<Menu> findByNameAndParentId(String text, long parentId);

	/**
	 * Same as store(Menu, boolean)
	 */
	public boolean store(Menu menu);

	/**
	 * For each menu, save the folder delete history entry for each folder and
	 * delete the folder
	 * 
	 * @param menu List of menu to be delete
	 */
	public void deleteAll(List<Menu> menus);

	/**
	 * Dynamically computes the path extended for the specified menu. The path
	 * extended is a human readable path in the form: /folder1/folder2/folder3
	 * 
	 * @param id
	 * @return
	 */
	public String computePathExtended(long id);

	/**
	 * Propagates the security policies of a node to the whole subree
	 */
	public boolean applyRithtToTree(long id);
}