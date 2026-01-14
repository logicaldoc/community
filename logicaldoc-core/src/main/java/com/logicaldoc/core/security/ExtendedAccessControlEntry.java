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
	protected boolean preview = true;

	@Column(name = "ld_download", nullable = false)
	protected boolean download = true;

	@Column(name = "ld_security", nullable = false)
	protected boolean security = false;

	@Column(name = "ld_delete", nullable = false)
	protected boolean delete = false;

	@Column(name = "ld_rename", nullable = false)
	protected boolean rename = false;

	@Column(name = "ld_immutable", nullable = false)
	protected boolean immutable = false;

	@Column(name = "ld_sign", nullable = false)
	protected boolean sign = false;

	@Column(name = "ld_archive", nullable = false)
	protected boolean archive = false;

	@Column(name = "ld_workflow", nullable = false)
	protected boolean workflow = false;

	@Column(name = "ld_calendar", nullable = false)
	protected boolean calendar = false;

	@Column(name = "ld_subscription", nullable = false)
	protected boolean subscription = false;

	@Column(name = "ld_password", nullable = false)
	protected boolean password = false;

	@Column(name = "ld_print", nullable = false)
	protected boolean print = true;

	@Column(name = "ld_move", nullable = false)
	protected boolean move = false;

	@Column(name = "ld_email", nullable = false)
	protected boolean email = true;

	@Column(name = "ld_automation", nullable = false)
	protected boolean automation = false;

	@Column(name = "ld_readingreq", nullable = false)
	protected boolean readingreq = false;

	@Column(name = "ld_customid", nullable = false)
	protected boolean customid = false;

	@Column(name = "ld_revision", nullable = false)
	protected boolean revision = false;

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

	public ExtendedAccessControlEntry(long groupId) {
		super(groupId);
		ace.setGroupId(groupId);
	}

	public AccessControlEntry getAce() {
		return ace;
	}

	public void setAce(AccessControlEntry ace) {
		this.ace = ace;
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
		return preview;
	}

	public void setPreview(boolean preview) {
		this.preview = preview;
	}

	public boolean isDownload() {
		return download;
	}

	public void setDownload(boolean download) {
		this.download = download;
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

	public boolean isRename() {
		return rename;
	}

	public void setRename(boolean rename) {
		this.rename = rename;
	}

	public boolean isImmutable() {
		return immutable;
	}

	public void setImmutable(boolean immutable) {
		this.immutable = immutable;
	}

	public boolean isSign() {
		return sign;
	}

	public void setSign(boolean sign) {
		this.sign = sign;
	}

	public boolean isArchive() {
		return archive;
	}

	public void setArchive(boolean archive) {
		this.archive = archive;
	}

	public boolean isWorkflow() {
		return workflow;
	}

	public void setWorkflow(boolean workflow) {
		this.workflow = workflow;
	}

	public boolean isCalendar() {
		return calendar;
	}

	public void setCalendar(boolean calendar) {
		this.calendar = calendar;
	}

	public boolean isSubscription() {
		return subscription;
	}

	public void setSubscription(boolean subscription) {
		this.subscription = subscription;
	}

	public boolean isPassword() {
		return password;
	}

	public void setPassword(boolean password) {
		this.password = password;
	}

	public boolean isPrint() {
		return print;
	}

	public void setPrint(boolean print) {
		this.print = print;
	}

	public boolean isMove() {
		return move;
	}

	public void setMove(boolean move) {
		this.move = move;
	}

	public boolean isEmail() {
		return email;
	}

	public void setEmail(boolean email) {
		this.email = email;
	}

	public boolean isAutomation() {
		return automation;
	}

	public void setAutomation(boolean automation) {
		this.automation = automation;
	}

	public boolean isReadingreq() {
		return readingreq;
	}

	public void setReadingreq(boolean readingreq) {
		this.readingreq = readingreq;
	}

	public boolean isCustomid() {
		return customid;
	}

	public void setCustomid(boolean customid) {
		this.customid = customid;
	}

	public boolean isRevision() {
		return revision;
	}

	public void setRevision(boolean revision) {
		this.revision = revision;
	}

	@Override
	public long getGroupId() {
		return ace.getGroupId();
	}

	@Override
	public void setGroupId(long groupId) {
		ace.setGroupId(groupId);
	}

	protected void grantedBasicPermissions(Set<Permission> granted) {
		if (read)
			granted.add(Permission.READ);
		if (preview)
			granted.add(Permission.PREVIEW);
		if (write)
			granted.add(Permission.WRITE);
		if (rename)
			granted.add(Permission.RENAME);
		if (delete)
			granted.add(Permission.DELETE);
		if (download)
			granted.add(Permission.DOWNLOAD);
		if (move)
			granted.add(Permission.MOVE);
		if (print)
			granted.add(Permission.PRINT);
		if (email)
			granted.add(Permission.EMAIL);
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		super.grantPermissions(permissions);
		ace.grantPermissions(permissions);
		preview = permissions.contains(Permission.PREVIEW);
		download = permissions.contains(Permission.DOWNLOAD);
		security = permissions.contains(Permission.SECURITY);
		delete = permissions.contains(Permission.DELETE);
		rename = permissions.contains(Permission.RENAME);
		immutable = permissions.contains(Permission.IMMUTABLE);
		sign = permissions.contains(Permission.SIGN);
		archive = permissions.contains(Permission.ARCHIVE);
		workflow = permissions.contains(Permission.WORKFLOW);
		calendar = permissions.contains(Permission.CALENDAR);
		subscription = permissions.contains(Permission.SUBSCRIPTION);
		password = permissions.contains(Permission.PASSWORD);
		print = permissions.contains(Permission.PRINT);
		move = permissions.contains(Permission.MOVE);
		email = permissions.contains(Permission.EMAIL);
		automation = permissions.contains(Permission.AUTOMATION);
		readingreq = permissions.contains(Permission.READINGREQ);
		customid = permissions.contains(Permission.CUSTOMID);
		revision = permissions.contains(Permission.REVISION);
	}

	@Override
	public Set<Permission> grantedPermissions() {
		Set<Permission> granted = ace.grantedPermissions();
		if (preview)
			granted.add(Permission.PREVIEW);
		if (download)
			granted.add(Permission.DOWNLOAD);
		if (security)
			granted.add(Permission.SECURITY);
		if (delete)
			granted.add(Permission.DELETE);
		if (rename)
			granted.add(Permission.RENAME);
		if (immutable)
			granted.add(Permission.IMMUTABLE);
		if (sign)
			granted.add(Permission.SIGN);
		if (archive)
			granted.add(Permission.ARCHIVE);
		if (workflow)
			granted.add(Permission.WORKFLOW);
		grantedMorePermissions(granted);
		return granted;
	}

	private void grantedMorePermissions(Set<Permission> granted) {
		if (calendar)
			granted.add(Permission.CALENDAR);
		if (subscription)
			granted.add(Permission.SUBSCRIPTION);
		if (password)
			granted.add(Permission.PASSWORD);
		if (print)
			granted.add(Permission.PRINT);
		if (move)
			granted.add(Permission.MOVE);
		if (email)
			granted.add(Permission.EMAIL);
		if (automation)
			granted.add(Permission.AUTOMATION);
		if (readingreq)
			granted.add(Permission.READINGREQ);
		if (customid)
			granted.add(Permission.CUSTOMID);
		if (revision)
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