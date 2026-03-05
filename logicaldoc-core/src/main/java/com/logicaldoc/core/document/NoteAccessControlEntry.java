package com.logicaldoc.core.document;

import java.util.Set;

import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.ExtendedAccessControlEntry;
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
	protected boolean security = false;

	@Column(name = "ld_delete", nullable = false)
	protected boolean delete = false;

	public NoteAccessControlEntry() {
	}

	public NoteAccessControlEntry(AccessControlEntry source) {
		this.ace = new AccessControlEntry(source);
		setGroupId(source.getGroupId());
		setRead(source.isRead());
		setWrite(source.isWrite());
	}

	public NoteAccessControlEntry(NoteAccessControlEntry source) {
		this.ace = new AccessControlEntry(source.getAce());
		setGroupId(source.getGroupId());
		setRead(source.isRead());
		setWrite(source.isWrite());
		setDelete(source.isDelete());
		setSecurity(source.isSecurity());
	}

	public NoteAccessControlEntry(ExtendedAccessControlEntry source) {
		this.ace = new AccessControlEntry(source.getAce());
		setGroupId(source.getGroupId());
		setRead(source.isRead());
		setWrite(source.isWrite());
		setDelete(source.isDelete());
		setSecurity(source.isSecurity());
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
		security = permissions.contains(Permission.SECURITY);
		delete = permissions.contains(Permission.DELETE);
	}

	public boolean isSecurity() {
		return security;
	}

	public void setSecurity(boolean security) {
		this.security = security;
	}

	public boolean isDelete() {
		return delete;
	}

	public void setDelete(boolean delete) {
		this.delete = delete;
	}

	@Override
	public long getGroupId() {
		return ace.getGroupId();
	}

	@Override
	public void setGroupId(long groupId) {
		ace.setGroupId(groupId);
	}

	@Override
	public boolean isRead() {
		return ace.isRead();
	}

	@Override
	public boolean isWrite() {
		return ace.isWrite();
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (security)
			granted.add(Permission.SECURITY);
		if (delete)
			granted.add(Permission.DELETE);
		return granted;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((ace == null) ? 0 : ace.hashCode());
		result = prime * result + (delete ? 1231 : 1237);
		return prime * result + (security ? 1231 : 1237);
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
		if (delete != other.delete)
			return false;
		return security == other.security;
	}

	@Override
	public String toString() {
		return "NoteAccessControlEntry [ace=" + ace + ", security=" + security + ", delete=" + delete + "]";
	}
}