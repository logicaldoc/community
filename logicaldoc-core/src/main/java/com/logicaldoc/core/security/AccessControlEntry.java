package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class AccessControlEntry implements Serializable {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_read", nullable = false)
	protected int read = 1;

	@Column(name = "ld_write", nullable = false)
	protected int write = 0;

	@Column(name = "ld_groupid", nullable = false)
	protected long groupId;

	public AccessControlEntry() {
	}

	public AccessControlEntry(AccessControlEntry source) {
		this.groupId = source.groupId;
		this.read = source.read;
		this.write = source.write;
	}

	public AccessControlEntry(long groupId) {
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

	public int getRead() {
		return read;
	}

	public void setRead(int read) {
		this.read = read;
	}

	public Set<Permission> grantedPermissions() {
		HashSet<Permission> granted = new HashSet<>();
		if (read == 1)
			granted.add(Permission.READ);

		if (write == 1)
			granted.add(Permission.WRITE);

		return granted;
	}

	public void grantPermissions(Set<Permission> permissions) {
		read = booleanToInt(permissions.contains(Permission.READ));
		write = booleanToInt(permissions.contains(Permission.WRITE));
	}

	protected int booleanToInt(boolean bool) {
		return bool ? 1 : 0;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof AccessControlEntry ace)
			return this.getGroupId() == ace.getGroupId();
		else
			return false;
	}

	@Override
	public int hashCode() {
		return Long.valueOf(groupId).hashCode();
	}

	@Override
	public String toString() {
		return "AccessControlEntry [read=" + read + ", write=" + write + ", groupId=" + groupId + "]";
	}
}