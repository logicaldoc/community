package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class AccessControlEntry implements Serializable {

	private static final long serialVersionUID = 1L;

	private int read = 1;

	private int write = 0;

	private int download = 1;

	private int add = 0;

	private int security = 0;

	private int delete = 0;

	private int rename = 0;

	private int immutable = 0;

	private int iimport = 0;

	private int export = 0;

	private int sign = 0;

	private int archive = 0;

	private int workflow = 0;

	private int calendar = 0;

	private int subscription = 0;

	private int password = 0;

	private int print = 1;

	private int move = 0;

	private int email = 1;

	private int automation = 0;

	private int storage = 0;

	private int readingreq = 0;

	private long groupId;

	public AccessControlEntry() {
	}

	public AccessControlEntry(AccessControlEntry source) {
		this.write = source.write;
		this.read = source.read;
		this.groupId = source.groupId;
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

	public int getDownload() {
		return download;
	}

	public void setDownload(int download) {
		this.download = download;
	}

	public int getAdd() {
		return add;
	}

	public void setAdd(int add) {
		this.add = add;
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

	public int getRename() {
		return rename;
	}

	public void setRename(int rename) {
		this.rename = rename;
	}

	public int getImmutable() {
		return immutable;
	}

	public void setImmutable(int immutable) {
		this.immutable = immutable;
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

	public int getSign() {
		return sign;
	}

	public void setSign(int sign) {
		this.sign = sign;
	}

	public int getArchive() {
		return archive;
	}

	public void setArchive(int archive) {
		this.archive = archive;
	}

	public int getWorkflow() {
		return workflow;
	}

	public void setWorkflow(int workflow) {
		this.workflow = workflow;
	}

	public int getCalendar() {
		return calendar;
	}

	public void setCalendar(int calendar) {
		this.calendar = calendar;
	}

	public int getSubscription() {
		return subscription;
	}

	public void setSubscription(int subscription) {
		this.subscription = subscription;
	}

	public int getPassword() {
		return password;
	}

	public void setPassword(int password) {
		this.password = password;
	}

	public int getPrint() {
		return print;
	}

	public void setPrint(int print) {
		this.print = print;
	}

	public int getMove() {
		return move;
	}

	public void setMove(int move) {
		this.move = move;
	}

	public int getEmail() {
		return email;
	}

	public void setEmail(int email) {
		this.email = email;
	}

	public int getAutomation() {
		return automation;
	}

	public void setAutomation(int automation) {
		this.automation = automation;
	}

	public int getStorage() {
		return storage;
	}

	public void setStorage(int storage) {
		this.storage = storage;
	}

	public int getReadingreq() {
		return readingreq;
	}

	public void setReadingreq(int readingreq) {
		this.readingreq = readingreq;
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

	public Set<Permission> grantedPermissions() {
		HashSet<Permission> granted = new HashSet<>();
		grantedBasicPermissions(granted);

		if (archive == 1)
			granted.add(Permission.ARCHIVE);
		if (automation == 1)
			granted.add(Permission.AUTOMATION);
		if (calendar == 1)
			granted.add(Permission.CALENDAR);
		if (export == 1)
			granted.add(Permission.EXPORT);
		if (iimport == 1)
			granted.add(Permission.IMPORT);
		if (immutable == 1)
			granted.add(Permission.IMMUTABLE);
		if (password == 1)
			granted.add(Permission.PASSWORD);
		if (readingreq == 1)
			granted.add(Permission.READINGREQ);
		if (security == 1)
			granted.add(Permission.SECURITY);
		if (sign == 1)
			granted.add(Permission.SIGN);
		if (storage == 1)
			granted.add(Permission.STORAGE);
		if (subscription == 1)
			granted.add(Permission.SUBSCRIPTION);
		if (workflow == 1)
			granted.add(Permission.WORKFLOW);

		return granted;
	}

	private void grantedBasicPermissions(HashSet<Permission> granted) {
		if (add == 1)
			granted.add(Permission.ADD);
		if (read == 1)
			granted.add(Permission.READ);
		if (write == 1)
			granted.add(Permission.WRITE);
		if (rename == 1)
			granted.add(Permission.RENAME);
		if (delete == 1)
			granted.add(Permission.DELETE);
		if (download == 1)
			granted.add(Permission.DOWNLOAD);
		if (move == 1)
			granted.add(Permission.MOVE);
		if (print == 1)
			granted.add(Permission.PRINT);
		if (email == 1)
			granted.add(Permission.EMAIL);
	}

	public void grantPermissions(Set<Permission> permissions) {
		add = booleanToInt(permissions.contains(Permission.ADD));
		archive = booleanToInt(permissions.contains(Permission.ARCHIVE));
		automation = booleanToInt(permissions.contains(Permission.AUTOMATION));
		calendar = booleanToInt(permissions.contains(Permission.CALENDAR));
		delete = booleanToInt(permissions.contains(Permission.DELETE));
		download = booleanToInt(permissions.contains(Permission.DOWNLOAD));
		email = booleanToInt(permissions.contains(Permission.EMAIL));
		export = booleanToInt(permissions.contains(Permission.EXPORT));
		iimport = booleanToInt(permissions.contains(Permission.IMPORT));
		immutable = booleanToInt(permissions.contains(Permission.IMMUTABLE));
		move = booleanToInt(permissions.contains(Permission.MOVE));
		password = booleanToInt(permissions.contains(Permission.PASSWORD));
		print = booleanToInt(permissions.contains(Permission.PRINT));
		readingreq = booleanToInt(permissions.contains(Permission.READINGREQ));
		read = booleanToInt(permissions.contains(Permission.READ));
		rename = booleanToInt(permissions.contains(Permission.RENAME));
		security = booleanToInt(permissions.contains(Permission.SECURITY));
		sign = booleanToInt(permissions.contains(Permission.SIGN));
		storage = booleanToInt(permissions.contains(Permission.STORAGE));
		subscription = booleanToInt(permissions.contains(Permission.SUBSCRIPTION));
		workflow = booleanToInt(permissions.contains(Permission.WORKFLOW));
		write = booleanToInt(permissions.contains(Permission.WRITE));
	}

	private int booleanToInt(boolean bool) {
		return bool ? 1 : 0;
	}
}