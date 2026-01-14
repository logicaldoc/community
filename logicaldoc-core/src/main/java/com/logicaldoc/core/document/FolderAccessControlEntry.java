package com.logicaldoc.core.document;

import java.util.Set;

import com.logicaldoc.core.security.ExtendedAccessControlEntry;
import com.logicaldoc.core.security.Permission;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.Embedded;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class FolderAccessControlEntry extends ExtendedAccessControlEntry {

	private static final long serialVersionUID = 1L;

	@Embedded
	private ExtendedAccessControlEntry ace = new ExtendedAccessControlEntry();

	@Column(name = "ld_add", nullable = false)
	private boolean add = false;

	@Column(name = "ld_import", nullable = false)
	private boolean iimport = false;

	@Column(name = "ld_export", nullable = false)
	private boolean export = false;

	@Column(name = "ld_store", nullable = false)
	private boolean store = false;

	public FolderAccessControlEntry() {
	}

	public FolderAccessControlEntry(FolderAccessControlEntry source) {
		super(source);
		ace = new ExtendedAccessControlEntry(source.getAce());
		this.add = source.add;
		this.iimport = source.iimport;
		this.export = source.export;
		this.store = source.store;
	}

	public FolderAccessControlEntry(long groupId) {
		super(groupId);
		setGroupId(groupId);
	}

	@Override
	public ExtendedAccessControlEntry getAce() {
		return ace;
	}

	public void setAce(ExtendedAccessControlEntry ace) {
		this.ace = ace;
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (add)
			granted.add(Permission.ADD);
		if (export)
			granted.add(Permission.EXPORT);
		if (iimport)
			granted.add(Permission.IMPORT);
		if (store)
			granted.add(Permission.STORE);
		return granted;
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		ace.grantPermissions(permissions);
		add = permissions.contains(Permission.ADD);
		export = permissions.contains(Permission.EXPORT);
		iimport = permissions.contains(Permission.IMPORT);
		store = permissions.contains(Permission.STORE);
	}

	public void setGroupId(long groupId) {
		ace.setGroupId(groupId);
	}

	public long getGroupId() {
		return ace.getGroupId();
	}

	public boolean isRead() {
		return ace.isRead();
	}

	public void setRead(boolean read) {
		ace.setRead(read);
	}

	public boolean isWrite() {
		return ace.isWrite();
	}

	public void setWrite(boolean write) {
		ace.setWrite(write);
	}

	public boolean isPreview() {
		return ace.isPreview();
	}

	public void setPreview(boolean preview) {
		ace.setPreview(preview);
	}

	public boolean isDownload() {
		return ace.isDownload();
	}

	public void setDownload(boolean download) {
		ace.setDownload(download);
	}

	public boolean isSecurity() {
		return ace.isSecurity();
	}

	public void setSecurity(boolean security) {
		ace.setSecurity(security);
	}

	public boolean isDelete() {
		return ace.isDelete();
	}

	public void setDelete(boolean delete) {
		ace.setDelete(delete);
	}

	public boolean isRename() {
		return ace.isRename();
	}

	public void setRename(boolean rename) {
		ace.setRename(rename);
	}

	public boolean isImmutable() {
		return ace.isImmutable();
	}

	public void setImmutable(boolean immutable) {
		ace.setImmutable(immutable);
	}

	public boolean isSign() {
		return ace.isSign();
	}

	public void setSign(boolean sign) {
		ace.setSign(sign);
	}

	public boolean isArchive() {
		return ace.isArchive();
	}

	public void setArchive(boolean archive) {
		ace.setArchive(archive);
	}

	public boolean isWorkflow() {
		return ace.isWorkflow();
	}

	public void setWorkflow(boolean workflow) {
		ace.setWorkflow(workflow);
	}

	public boolean isCalendar() {
		return ace.isCalendar();
	}

	public void setCalendar(boolean calendar) {
		ace.setCalendar(calendar);
	}

	public boolean isSubscription() {
		return ace.isSubscription();
	}

	public void setSubscription(boolean subscription) {
		ace.setSubscription(subscription);
	}

	public boolean isPassword() {
		return ace.isPassword();
	}

	public void setPassword(boolean password) {
		ace.setPassword(password);
	}

	public boolean isPrint() {
		return ace.isPrint();
	}

	public void setPrint(boolean print) {
		ace.setPrint(print);
	}

	public boolean isMove() {
		return ace.isMove();
	}

	public void setMove(boolean move) {
		ace.setMove(move);
	}

	public boolean isEmail() {
		return ace.isEmail();
	}

	public void setEmail(boolean email) {
		ace.setEmail(email);
	}

	public boolean isAutomation() {
		return ace.isAutomation();
	}

	public void setAutomation(boolean automation) {
		ace.setAutomation(automation);
	}

	public boolean isReadingreq() {
		return ace.isReadingreq();
	}

	public void setReadingreq(boolean readingreq) {
		ace.setReadingreq(readingreq);
	}

	public boolean isCustomid() {
		return ace.isCustomid();
	}

	public void setCustomid(boolean customid) {
		ace.setCustomid(customid);
	}

	public boolean isRevision() {
		return ace.isRevision();
	}

	public void setRevision(boolean revision) {
		ace.setRevision(revision);
	}

	public boolean isAdd() {
		return add;
	}

	public void setAdd(boolean add) {
		this.add = add;
	}

	public boolean isImport() {
		return iimport;
	}

	public void setImport(boolean imprt) {
		this.iimport = imprt;
	}
	
	public boolean isIimport() {
		return iimport;
	}

	public void setIimport(boolean iimport) {
		this.iimport = iimport;
	}

	public boolean isExport() {
		return export;
	}

	public void setExport(boolean export) {
		this.export = export;
	}

	public boolean isStore() {
		return store;
	}

	public void setStore(boolean store) {
		this.store = store;
	}

	@Override
	public String toString() {
		return "FolderAccessControlEntry [ace=" + ace + ", add=" + add + ", iimport=" + iimport + ", export=" + export
				+ ", store=" + store + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((ace == null) ? 0 : ace.hashCode());
		result = prime * result + (add ? 1231 : 1237);
		result = prime * result + (export ? 1231 : 1237);
		result = prime * result + (iimport ? 1231 : 1237);
		result = prime * result + (store ? 1231 : 1237);
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
		FolderAccessControlEntry other = (FolderAccessControlEntry) obj;
		if (ace == null) {
			if (other.ace != null)
				return false;
		} else if (!ace.equals(other.ace))
			return false;
		if (add != other.add)
			return false;
		if (export != other.export)
			return false;
		if (iimport != other.iimport)
			return false;
		return store == other.store;
	}
}