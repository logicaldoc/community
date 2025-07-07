package com.logicaldoc.core.security.menu;

import java.util.HashSet;
import java.util.Set;

import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Secure;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.Table;

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
@Entity
@Table(name = "ld_menu")
@Cacheable
public class Menu extends PersistentObject implements Secure<AccessControlEntry>, Comparable<Menu> {

	private static final long serialVersionUID = 1L;

	public static final long ROOT = 1;

	public static final long ADMINISTRATION = 2;

	public static final long FRONTEND = 5;

	public static final long SETTINGS = 7;

	public static final long SECURITY = 9;
	
	public static final long TOOLS = 16;

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

	public static final long AUDITING = 106;

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

	@Column(name = "ld_name", length = 255)
	private String name = "";

	@Column(name = "ld_position", nullable = false)
	private int position = 1;

	@Column(name = "ld_parentid", nullable = false)
	private long parentId = 0;

	@Column(name = "ld_icon", length = 255)
	private String icon = "";

	@Column(name = "ld_type", nullable = false)
	private int type = TYPE_DEFAULT;

	@Column(name = "ld_enabled", nullable = false)
	private int enabled = 1;

	@Column(name = "ld_description", length = 4000)
	private String description = "";

	/**
	 * The declared routine to execute
	 */
	@Column(name = "ld_routineid")
	private Long routineId;

	/**
	 * Automation script to execute(in absence of routine specification)
	 */
	@Column(name = "ld_automation")
	private String automation;

	@ElementCollection(fetch = FetchType.EAGER)
	@CollectionTable(name = "ld_menu_acl", joinColumns = @JoinColumn(name = "ld_menuid"))
	private Set<AccessControlEntry> accessControlList = new HashSet<>();

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
	public void setAccessControlList(Set<AccessControlEntry> acl) {
		accessControlList = acl;
	}

	@Override
	public Set<AccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return getAccessControlList().stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public void addAccessControlEntry(AccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
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
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + (int) (parentId ^ (parentId >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Menu other = (Menu) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return parentId == other.parentId;
	}
}