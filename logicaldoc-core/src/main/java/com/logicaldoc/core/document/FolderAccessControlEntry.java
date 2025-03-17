package com.logicaldoc.core.document;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import com.logicaldoc.core.security.ExtendedAccessControlEntry;
import com.logicaldoc.core.security.Permission;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class FolderAccessControlEntry extends ExtendedAccessControlEntry {

	private static final long serialVersionUID = 1L;

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
		this.add = source.add;
		this.iimport = source.iimport;
		this.export = source.export;
		this.store = source.store;
	}

	public FolderAccessControlEntry(long groupId) {
		super(groupId);
	}

	@Override
	public Set<Permission> grantedPermissions() {
		HashSet<Permission> granted = new HashSet<>();
		grantedBasicPermissions(granted);

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
}
