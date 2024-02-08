package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

import com.logicaldoc.gui.common.client.Constants;

public class GUIAutomationRoutine extends GUIExtensibleObject implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	private String description;

	private String automation;

	private GUIAccessControlEntry[] rights = new GUIAccessControlEntry[] {};

	private String[] permissions = new String[] {};

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

	public GUIAccessControlEntry[] getRights() {
		return rights;
	}

	public void setRights(GUIAccessControlEntry[] rights) {
		this.rights = rights;
	}

	public String[] getPermissions() {
		return permissions;
	}

	public void setPermissions(String[] permissions) {
		this.permissions = permissions;
	}

	public boolean isWrite() {
		return hasPermission(Constants.PERMISSION_WRITE);
	}

	public boolean hasPermission(String permission) {
		if (permissions == null)
			return false;
		for (String p : permissions)
			if (p.equals(permission))
				return true;
		return false;
	}
}