package com.logicaldoc.core.security;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.core.PersistentObject;

/**
 * This class represents the key concept of security. A Menu not only models
 * menus but also it is used as an element to build hierarchies. With menugroups
 * you can associate groups to a given menu and grant some permissions. Also
 * setting the recurityRef you can specify another reference menu that contains
 * the security policies.
 * 
 * @author Michael Scholz
 * @author Marco Meschieri - Logical Objects
 * @version 1.0
 */
public class Menu extends PersistentObject implements Comparable<Menu> {

	public static long ROOT = 1;

	public static long ADMINISTRATION = 2;
	
	public static long SECURITY = 9;
	
	public static long DOCUMENTS = 1500;

	public static long SESSIONS = 1601;
	
	public static long SETTINGS = 7;
	
	public static long PARAMETERS = 100;
	
	public static long ADMIN_SESSIONS = 71;
	
	public static long LOGS = 72;

	private long id = 0;

	private String name = "";

	private long parentId = 0;

	private Long securityRef;

	private String icon = "";

	private int type = 1;

	private String description = "";

	private int position = 1;

	protected Set<MenuGroup> menuGroups = new HashSet<MenuGroup>();

	public Menu() {
	}

	public long getId() {
		return id;
	}

	public long getParentId() {
		return parentId;
	}

	public String getIcon() {
		return icon;
	}

	public int getType() {
		return type;
	}

	public Set<MenuGroup> getMenuGroups() {
		return menuGroups;
	}

	public void clearMenuGroups() {
		menuGroups.clear();
		menuGroups = new HashSet<MenuGroup>();
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public void setType(int type) {
		this.type = type;
	}

	public void setMenuGroups(Set<MenuGroup> mgroup) {
		menuGroups = mgroup;
	}

	public long[] getMenuGroupIds() {
		long[] idsArray = new long[menuGroups.size()];
		int i = 0;
		for (MenuGroup mg : menuGroups) {
			idsArray[i++] = mg.getGroupId();
		}
		return idsArray;
	}

	/**
	 * Adds MenuGroup object given in a String array to the ArrayList of
	 * MenuGroups.
	 * 
	 * @param groups array of group ids
	 */
	public void setMenuGroup(long[] groups) {
		menuGroups.clear();
		for (int i = 0; i < groups.length; i++) {
			MenuGroup mg = new MenuGroup();
			mg.setGroupId(groups[i]);
			mg.setWrite(1);
			mg.setManageSecurity(1);
			mg.setDelete(1);
			mg.setRename(1);
			menuGroups.add(mg);
		}
	}

	/**
	 * Adds a new element, substituting a precedin one with the same groupId.
	 */
	public void addMenuGroup(MenuGroup mg) {
		MenuGroup m = getMenuGroup(mg.getGroupId());
		getMenuGroups().remove(m);
		getMenuGroups().add(mg);
	}

	public MenuGroup getMenuGroup(long groupId) {
		for (MenuGroup mg : menuGroups) {
			if (mg.getGroupId() == groupId)
				return mg;
		}
		return null;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Long getSecurityRef() {
		return securityRef;
	}

	public void setSecurityRef(Long securityRef) {
		this.securityRef = securityRef;
	}

	@Override
	public int compareTo(Menu o) {
		int comparation = Integer.compare(this.position, o.position);
		if (comparation != 0)
			return comparation;
		return this.name.compareTo(o.name);
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}