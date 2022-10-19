package com.logicaldoc.core.metadata;

import java.io.Serializable;

import com.logicaldoc.core.security.Permission;

/**
 * This class represents security permissions for a group in relation to a
 * template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.2
 */
public class TemplateGroup implements Serializable {

	private static final long serialVersionUID = 1L;

	private int write = 0;

	private long groupId;

	public TemplateGroup() {
	}

	public TemplateGroup(long groupId) {
		this.groupId = groupId;
	}

	public TemplateGroup(TemplateGroup source) {
		this.write = source.write;
		this.groupId = source.groupId;
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
	public boolean equals(Object obj) {
		if (!(obj instanceof TemplateGroup))
			return false;
		TemplateGroup other = (TemplateGroup) obj;
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
		return Integer.parseInt(sb.toString(), 2);
	}

	/**
	 * Set each permission evaluating the given integer representation.
	 * 
	 * @param permissions mask(the last slot is for the 'read' permission and it
	 *        is not evaluated)
	 */
	public void setPermissions(int permissions) {
		setWrite(Permission.WRITE.match(permissions) ? 1 : 0);
	}
}