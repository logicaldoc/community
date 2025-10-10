package com.logicaldoc.core.document;

import java.util.Set;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Permission;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.Embedded;

/**
 * Class for permissions of {@link DocumentNote}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
@Embeddable
public class NoteAccessControlEntry extends AccessControlEntry {

	private static final long serialVersionUID = 1L;

	@Embedded
	private AccessControlEntry ace = new AccessControlEntry();

	@Column(name = "ld_security", nullable = false)
	protected int security = 0;

	@Column(name = "ld_delete", nullable = false)
	protected int delete = 0;

	public NoteAccessControlEntry() {
	}

	public NoteAccessControlEntry(NoteAccessControlEntry source) {
		super(source);
		this.ace = new AccessControlEntry(source.getAce());
		this.delete = source.delete;
		this.security = source.security;
	}

	public NoteAccessControlEntry(long groupId) {
		super(groupId);
		ace.setGroupId(groupId);
	}

	public AccessControlEntry getAce() {
		return ace;
	}

	public void setAce(AccessControlEntry ace) {
		this.ace = ace;
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		ace.grantPermissions(permissions);
		security = booleanToInt(permissions.contains(Permission.SECURITY));
		delete = booleanToInt(permissions.contains(Permission.DELETE));
	}

	public int getSecurity() {
		return security;
	}

	public void setSecurity(int security) {
		this.security = security;
	}

	public int getDelete() {
		return delete;
	}

	public void setDelete(int delete) {
		this.delete = delete;
	}

	@Override
	public long getGroupId() {
		return ace.getGroupId();
	}

	@Override
	public int getWrite() {
		return ace.getWrite();
	}

	@Override
	public void setGroupId(long groupId) {
		ace.setGroupId(groupId);
	}

	@Override
	public void setWrite(int write) {
		ace.setWrite(write);
	}

	@Override
	public int getRead() {
		return ace.getRead();
	}

	@Override
	public void setRead(int read) {
		ace.setRead(read);
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (security == 1)
			granted.add(Permission.SECURITY);
		if (delete == 1)
			granted.add(Permission.DELETE);
		return granted;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((ace == null) ? 0 : ace.hashCode());
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
		NoteAccessControlEntry other = (NoteAccessControlEntry) obj;
		if (ace == null) {
			if (other.ace != null)
				return false;
		} else if (!ace.equals(other.ace))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "NoteAccessControlEntry [ace=" + ace + ", security=" + security + ", delete=" + delete + "]";
	}
}