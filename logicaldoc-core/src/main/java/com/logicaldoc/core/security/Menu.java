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
 * @author Marco Meschieri - LogicalDOC
 * @version 1.0
 */
public class Menu extends PersistentObject implements Comparable<Menu> {

	private static final long serialVersionUID = 1L;
	
	public static final long ROOT = 1;

	public static final long ADMINISTRATION = 2;

	public static final long FRONTEND = 5;

	public static final long SETTINGS = 7;

	public static final long SECURITY = 9;

	public static final long MAINMENU = 110;

	public static final long DOCUMENTS = 1500;

	public static final long SEARCH = 1510;

	public static final long DASHBOARD = 1520;

	public static final long MESSAGES = 1525;

	public static final long SESSIONS = 1601;

	public static final long VERSIONS = 1603;

	public static final long ALIASES = 1605;
	
	public static final long PREVIEW = 1609;
	
	public static final long RATING = 1610;

	public static final long PARAMETERS = 100;
	
	public static final long ADMIN_SESSIONS = 71;

	public static final long LOGS = 72;

	public static final long SUBSCRIPTIONS = -1120;

	public static final long ACCOUNT = 40;

	public static final long CONTACTS = 1530;

	public static final long CHAT = 1527;

	public static final long INTERFACE_DENSITY = 1535;

	public static final long CUSTOM_ACTIONS = 1300;
	
	public static final int TYPE_DEFAULT = 1;

	public static final int TYPE_CUSTOM_ACTION = 2;

	private long id = 0;

	private String name = "";

	private long parentId = 0;

	private Long securityRef;

	private String icon = "";

	private int type = TYPE_DEFAULT;

	private String description = "";

	private int position = 1;

	private int enabled = 1;
	
	/**
	 * The declared routine to execute
	 */
	private Long routineId;

	/**
	 * Automation script to execute(in absence of routine specification)
	 */
	private String automation;

	private Set<MenuGroup> menuGroups = new HashSet<MenuGroup>();

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
	 * Adds a new element, substituting a previous one with the same groupId.
	 * 
	 * @param mg the menu group
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
		int comparison = Integer.compare(this.position, o.position);
		if (comparison != 0)
			return comparison;
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

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public Long getRoutineId() {
		return routineId;
	}

	public void setRoutineId(Long routineId) {
		this.routineId = routineId;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}
}