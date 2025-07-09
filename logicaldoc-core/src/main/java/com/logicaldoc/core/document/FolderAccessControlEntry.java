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
	private int add = 0;

	@Column(name = "ld_import", nullable = false)
	private int iimport = 0;

	@Column(name = "ld_export", nullable = false)
	private int export = 0;

	@Column(name = "ld_store", nullable = false)
	private int store = 0;

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

	public ExtendedAccessControlEntry getAce() {
		return ace;
	}

	public void setAce(ExtendedAccessControlEntry ace) {
		this.ace = ace;
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (add == 1)
			granted.add(Permission.ADD);
		if (export == 1)
			granted.add(Permission.EXPORT);
		if (iimport == 1)
			granted.add(Permission.IMPORT);
		if (store == 1)
			granted.add(Permission.STORE);
		return granted;
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		ace.grantPermissions(permissions);
		add = booleanToInt(permissions.contains(Permission.ADD));
		export = booleanToInt(permissions.contains(Permission.EXPORT));
		iimport = booleanToInt(permissions.contains(Permission.IMPORT));
		store = booleanToInt(permissions.contains(Permission.STORE));
	}

	public int getAdd() {
		return add;
	}

	public void setAdd(int add) {
		this.add = add;
	}

	public int getImport() {
		return iimport;
	}

	public void setImport(int iimport) {
		this.iimport = iimport;
	}

	public int getExport() {
		return export;
	}

	public void setExport(int export) {
		this.export = export;
	}

	public int getStore() {
		return store;
	}

	public void setStore(int store) {
		this.store = store;
	}

	@Override
	public int getPreview() {
		return ace.getPreview();
	}

	@Override
	public void setPreview(int preview) {
		ace.setPreview(preview);
	}

	@Override
	public int getDownload() {
		return ace.getDownload();
	}

	@Override
	public void setDownload(int download) {
		ace.setDownload(download);
	}

	@Override
	public int getSecurity() {
		return ace.getSecurity();
	}

	@Override
	public void setSecurity(int security) {
		ace.setSecurity(security);
	}

	@Override
	public int getDelete() {
		return ace.getDelete();
	}

	@Override
	public void setDelete(int delete) {
		ace.setDelete(delete);
	}

	@Override
	public int getRename() {
		return ace.getRename();
	}

	@Override
	public void setRename(int rename) {
		ace.setRename(rename);
	}

	@Override
	public int getImmutable() {
		return ace.getImmutable();
	}

	@Override
	public void setImmutable(int immutable) {
		ace.setImmutable(immutable);
	}

	@Override
	public int getSign() {
		return ace.getSign();
	}

	@Override
	public void setSign(int sign) {
		ace.setSign(sign);
	}

	@Override
	public int getArchive() {
		return ace.getArchive();
	}

	@Override
	public void setArchive(int archive) {
		ace.setArchive(archive);
	}

	@Override
	public int getWorkflow() {
		return ace.getWorkflow();
	}

	@Override
	public void setWorkflow(int workflow) {
		ace.setWorkflow(workflow);
	}

	@Override
	public int getCalendar() {
		return ace.getCalendar();
	}

	@Override
	public void setCalendar(int calendar) {
		ace.setCalendar(calendar);
	}

	@Override
	public int getSubscription() {
		return ace.getSubscription();
	}

	@Override
	public void setSubscription(int subscription) {
		ace.setSubscription(subscription);
	}

	@Override
	public int getPassword() {
		return ace.getPassword();
	}

	@Override
	public void setPassword(int password) {
		ace.setPassword(password);
	}

	@Override
	public int getPrint() {
		return ace.getPrint();
	}

	@Override
	public void setPrint(int print) {
		ace.setPrint(print);
	}

	@Override
	public int getMove() {
		return ace.getMove();
	}

	@Override
	public void setMove(int move) {
		ace.setMove(move);
	}

	@Override
	public int getEmail() {
		return ace.getEmail();
	}

	@Override
	public void setEmail(int email) {
		ace.setEmail(email);
	}

	@Override
	public int getAutomation() {
		return ace.getAutomation();
	}

	@Override
	public void setAutomation(int automation) {
		ace.setAutomation(automation);
	}

	@Override
	public int getReadingreq() {
		return ace.getReadingreq();
	}

	@Override
	public void setReadingreq(int readingreq) {
		ace.setReadingreq(readingreq);
	}

	@Override
	public int getCustomid() {
		return ace.getCustomid();
	}

	@Override
	public void setCustomid(int customid) {
		ace.setCustomid(customid);
	}

	@Override
	public int getRevision() {
		return ace.getRevision();
	}

	@Override
	public void setRevision(int revision) {
		ace.setRevision(revision);
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
	public String toString() {
		return ace.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((ace == null) ? 0 : ace.hashCode());
		result = prime * result + add;
		result = prime * result + export;
		result = prime * result + iimport;
		result = prime * result + store;
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
		if (store != other.store)
			return false;
		return true;
	}

}