package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class GUIAutomationRoutine extends GUIExtensibleObject implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	private String description;

	private String automation;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	private List<String> permissions = new ArrayList<>();

	public GUIAutomationRoutine(long id) {
		super(id);
	}

	public GUIAutomationRoutine() {

	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public GUIAccessControlEntry getAce(long entityId) {
		for (GUIAccessControlEntry ace : accessControlList) {
			if (ace.getEntityId() == entityId)
				return ace;
		}
		return null;
	}

	public void removeAce(long entityId) {
		List<GUIAccessControlEntry> newAcls = new ArrayList<>();
		for (GUIAccessControlEntry ace : accessControlList) {
			if (ace.getEntityId() != entityId)
				newAcls.add(ace);
		}
		accessControlList = newAcls;
	}

	public void addAce(GUIAccessControlEntry ace) {
		GUIAccessControlEntry existingAce = getAce(ace.getEntityId());
		if(existingAce==null) {
			accessControlList.add(ace);
		} else {
			existingAce.setRead(ace.isRead());
			existingAce.setWrite(ace.isWrite());
		}
	}
	
	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public List<String> getPermissions() {
		return permissions;
	}

	public void setPermissions(List<String> permissions) {
		this.permissions = permissions;
	}

	public boolean isWrite() {
		return hasPermission(GUIAccessControlEntry.PERMISSION_WRITE);
	}

	public boolean hasPermission(String permission) {
		if (permissions == null)
			return false;
		for (String p : permissions)
			if (p.equals(permission))
				return true;
		return false;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((automation == null) ? 0 : automation.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		GUIAutomationRoutine other = (GUIAutomationRoutine) obj;
		if (automation == null) {
			if (other.automation != null)
				return false;
		} else if (!automation.equals(other.automation))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}