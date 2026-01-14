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
	protected boolean read = true;

	@Column(name = "ld_write", nullable = false)
	protected boolean write = false;

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

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	public long getGroupId() {
		return groupId;
	}
	
	public boolean isRead() {
		return read;
	}

	public void setRead(boolean read) {
		this.read = read;
	}

	public boolean isWrite() {
		return write;
	}

	public void setWrite(boolean write) {
		this.write = write;
	}

	public Set<Permission> grantedPermissions() {
		HashSet<Permission> granted = new HashSet<>();
		if (read)
			granted.add(Permission.READ);

		if (write)
			granted.add(Permission.WRITE);

		return granted;
	}

	public void grantPermissions(Set<Permission> permissions) {
		read = permissions.contains(Permission.READ);
		write = permissions.contains(Permission.WRITE);
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