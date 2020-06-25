package com.logicaldoc.core.security;

/**
 * This class represents security permissions for a group in relation to a menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 1.0
 */
public class MenuGroup {

	private int write = 0;

	private int manageSecurity = 0;

	private int delete = 0;

	private int rename = 0;

	private long groupId;

	public MenuGroup() {
	}

	public int getManageSecurity() {
		return manageSecurity;
	}

	public void setManageSecurity(int manageSecurity) {
		this.manageSecurity = manageSecurity;
	}

	public int getDelete() {
		return delete;
	}

	public void setDelete(int delete) {
		this.delete = delete;
	}

	public int getRename() {
		return rename;
	}

	public void setRename(int rename) {
		this.rename = rename;
	}

	public MenuGroup(long groupId) {
		this.groupId = groupId;
	}

	public long getGroupId() {
		return groupId;
	}

	public int getWrite() {
		return write;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	public void setWrite(int write) {
		this.write = write;
	}

	@Override
	public MenuGroup clone() {
		MenuGroup mg = new MenuGroup(groupId);
		mg.setDelete(delete);
		mg.setManageSecurity(manageSecurity);
		mg.setRename(rename);
		mg.setWrite(write);
		return mg;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof MenuGroup))
			return false;
		MenuGroup other = (MenuGroup) obj;
		return this.getGroupId() == other.getGroupId();
	}

	@Override
	public int hashCode() {
		return Long.valueOf(groupId).hashCode();
	}

	/**
	 * Parsing each permission and creates the integer representation
	 * 
	 * @return Permissions settings as integer representation.
	 */
	public int getPermissions() {
		StringBuffer sb = new StringBuffer("1");
		sb.append(getWrite() == 1 ? "1" : "0");
		sb.append(getManageSecurity() == 1 ? "1" : "0");
		sb.append(getDelete() == 1 ? "1" : "0");
		sb.append(getRename() == 1 ? "1" : "0");

		return Integer.parseInt(sb.toString(), 2);
	}

	/**
	 * Set each permission evaluating the given integer representation.
	 * 
	 * @param permissions mask(the last slot is for the 'read'
	 *        permission and it is not evaluated)
	 */
	public void setPermissions(int permissions) {
		setWrite(Permission.WRITE.match(permissions) ? 1 : 0);
		setManageSecurity(Permission.SECURITY.match(permissions) ? 1 : 0);
		setDelete(Permission.DELETE.match(permissions) ? 1 : 0);
		setRename(Permission.RENAME.match(permissions) ? 1 : 0);
	}
}