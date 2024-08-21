package com.logicaldoc.core.security.menu;

import com.logicaldoc.core.security.SecurablePersistentObject;

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
public class Menu extends SecurablePersistentObject implements Comparable<Menu> {

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

	public static final long DESTROY_DOCUMENTS = -9;

	private String name = "";

	private long parentId = 0;

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

	public Menu() {
		super();
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

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	@Override
	public int compareTo(Menu other) {
		if (this.equals(other))
			return 0;

		int comparison = Integer.compare(this.position, other.position);
		if (comparison != 0)
			return comparison;
		return this.name.compareTo(other.name);
	}

	@Override
	public int hashCode() {
		int result;
		result = getClass().getName().hashCode();
		result = 29 * result + Long.valueOf(getId()).hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null)
			return false;

		if (obj instanceof Menu other)
			return other.getId() == this.getId();
		else
			return false;
	}
}