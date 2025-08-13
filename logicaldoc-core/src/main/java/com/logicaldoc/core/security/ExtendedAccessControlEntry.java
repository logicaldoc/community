package com.logicaldoc.core.security;

import java.util.Set;

import com.logicaldoc.core.document.DocumentAccessControlEntry;
import com.logicaldoc.core.document.FolderAccessControlEntry;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.Embedded;

/**
 * Class for grouping common permissions for {@link DocumentAccessControlEntry}
 * and {@link FolderAccessControlEntry}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2
 */
@Embeddable
public class ExtendedAccessControlEntry extends AccessControlEntry {

	private static final long serialVersionUID = 1L;

	@Embedded
	private AccessControlEntry ace = new AccessControlEntry();

	@Column(name = "ld_preview", nullable = false)
	protected int preview = 1;

	@Column(name = "ld_download", nullable = false)
	protected int download = 1;

	@Column(name = "ld_security", nullable = false)
	protected int security = 0;

	@Column(name = "ld_delete", nullable = false)
	protected int delete = 0;

	@Column(name = "ld_rename", nullable = false)
	protected int rename = 0;

	@Column(name = "ld_immutable", nullable = false)
	protected int immutable = 0;

	@Column(name = "ld_sign", nullable = false)
	protected int sign = 0;

	@Column(name = "ld_archive", nullable = false)
	protected int archive = 0;

	@Column(name = "ld_workflow", nullable = false)
	protected int workflow = 0;

	@Column(name = "ld_calendar", nullable = false)
	protected int calendar = 0;

	@Column(name = "ld_subscription", nullable = false)
	protected int subscription = 0;

	@Column(name = "ld_password", nullable = false)
	protected int password = 0;

	@Column(name = "ld_print", nullable = false)
	protected int print = 1;

	@Column(name = "ld_move", nullable = false)
	protected int move = 0;

	@Column(name = "ld_email", nullable = false)
	protected int email = 1;

	@Column(name = "ld_automation", nullable = false)
	protected int automation = 0;

	@Column(name = "ld_readingreq", nullable = false)
	protected int readingreq = 0;

	@Column(name = "ld_customid", nullable = false)
	protected int customid = 0;

	@Column(name = "ld_revision", nullable = false)
	protected int revision = 0;

	public ExtendedAccessControlEntry() {
	}

	public ExtendedAccessControlEntry(ExtendedAccessControlEntry source) {
		super(source);
		this.ace = new AccessControlEntry(source.getAce());
		this.archive = source.archive;
		this.automation = source.automation;
		this.calendar = source.calendar;
		this.delete = source.delete;
		this.download = source.download;
		this.email = source.email;
		this.immutable = source.immutable;
		this.move = source.move;
		this.password = source.password;
		this.preview = source.preview;
		this.print = source.print;
		this.rename = source.rename;
		this.security = source.security;
		this.sign = source.sign;
		this.subscription = source.subscription;
		this.workflow = source.workflow;
		this.readingreq = source.readingreq;
		this.customid = source.customid;
		this.revision = source.revision;
	}

	protected ExtendedAccessControlEntry(long groupId) {
		super(groupId);
		ace.setGroupId(groupId);
	}

	public AccessControlEntry getAce() {
		return ace;
	}

	public void setAce(AccessControlEntry ace) {
		this.ace = ace;
	}

	protected void grantedBasicPermissions(Set<Permission> granted) {
		if (read == 1)
			granted.add(Permission.READ);
		if (preview == 1)
			granted.add(Permission.PREVIEW);
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

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		ace.grantPermissions(permissions);
		preview = booleanToInt(permissions.contains(Permission.PREVIEW));
		download = booleanToInt(permissions.contains(Permission.DOWNLOAD));
		security = booleanToInt(permissions.contains(Permission.SECURITY));
		delete = booleanToInt(permissions.contains(Permission.DELETE));
		rename = booleanToInt(permissions.contains(Permission.RENAME));
		immutable = booleanToInt(permissions.contains(Permission.IMMUTABLE));
		sign = booleanToInt(permissions.contains(Permission.SIGN));
		archive = booleanToInt(permissions.contains(Permission.ARCHIVE));
		workflow = booleanToInt(permissions.contains(Permission.WORKFLOW));
		calendar = booleanToInt(permissions.contains(Permission.CALENDAR));
		subscription = booleanToInt(permissions.contains(Permission.SUBSCRIPTION));
		password = booleanToInt(permissions.contains(Permission.PASSWORD));
		print = booleanToInt(permissions.contains(Permission.PRINT));
		move = booleanToInt(permissions.contains(Permission.MOVE));
		email = booleanToInt(permissions.contains(Permission.EMAIL));
		automation = booleanToInt(permissions.contains(Permission.AUTOMATION));
		readingreq = booleanToInt(permissions.contains(Permission.READINGREQ));
		customid = booleanToInt(permissions.contains(Permission.CUSTOMID));
		revision = booleanToInt(permissions.contains(Permission.REVISION));
	}

	public int getPreview() {
		return preview;
	}

	public void setPreview(int preview) {
		this.preview = preview;
	}

	public int getDownload() {
		return download;
	}

	public void setDownload(int download) {
		this.download = download;
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

	public int getReadingreq() {
		return readingreq;
	}

	public void setReadingreq(int readingreq) {
		this.readingreq = readingreq;
	}

	public int getCustomid() {
		return customid;
	}

	public void setCustomid(int customid) {
		this.customid = customid;
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

	public int getRevision() {
		return revision;
	}

	public void setRevision(int revision) {
		this.revision = revision;
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (preview == 1)
			granted.add(Permission.PREVIEW);
		if (download == 1)
			granted.add(Permission.DOWNLOAD);
		if (security == 1)
			granted.add(Permission.SECURITY);
		if (delete == 1)
			granted.add(Permission.DELETE);
		if (rename == 1)
			granted.add(Permission.RENAME);
		if (immutable == 1)
			granted.add(Permission.IMMUTABLE);
		if (sign == 1)
			granted.add(Permission.SIGN);
		if (archive == 1)
			granted.add(Permission.ARCHIVE);
		if (workflow == 1)
			granted.add(Permission.WORKFLOW);
		grantedMorePermissions(granted);
		return granted;
	}

	private void grantedMorePermissions(Set<Permission> granted) {
		if (calendar == 1)
			granted.add(Permission.CALENDAR);
		if (subscription == 1)
			granted.add(Permission.SUBSCRIPTION);
		if (password == 1)
			granted.add(Permission.PASSWORD);
		if (print == 1)
			granted.add(Permission.PRINT);
		if (move == 1)
			granted.add(Permission.MOVE);
		if (email == 1)
			granted.add(Permission.EMAIL);
		if (automation == 1)
			granted.add(Permission.AUTOMATION);
		if (readingreq == 1)
			granted.add(Permission.READINGREQ);
		if (customid == 1)
			granted.add(Permission.CUSTOMID);
		if (revision == 1)
			granted.add(Permission.REVISION);
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
		ExtendedAccessControlEntry other = (ExtendedAccessControlEntry) obj;
		if (ace == null) {
			if (other.ace != null)
				return false;
		} else if (!ace.equals(other.ace))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExtendedAccessControlEntry [ace=" + ace + ", preview=" + preview + ", download=" + download
				+ ", security=" + security + ", delete=" + delete + ", rename=" + rename + ", immutable=" + immutable
				+ ", sign=" + sign + ", archive=" + archive + ", workflow=" + workflow + ", calendar=" + calendar
				+ ", subscription=" + subscription + ", password=" + password + ", print=" + print + ", move=" + move
				+ ", email=" + email + ", automation=" + automation + ", readingreq=" + readingreq + ", customid="
				+ customid + ", revision=" + revision + "]";
	}

}