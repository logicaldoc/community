package com.logicaldoc.core.document;

import java.util.Set;

import com.logicaldoc.core.security.ExtendedAccessControlEntry;
import com.logicaldoc.core.security.Permission;

import jakarta.persistence.Embeddable;
import jakarta.persistence.Embedded;

/**
 * Represents all the permissions granted to a group against a business object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
@Embeddable
public class DocumentAccessControlEntry extends ExtendedAccessControlEntry {

	private static final long serialVersionUID = 1L;

	@Embedded
	private ExtendedAccessControlEntry ace = new ExtendedAccessControlEntry();

	public DocumentAccessControlEntry() {
	}

	public DocumentAccessControlEntry(DocumentAccessControlEntry source) {
		super(source);
		ace = new ExtendedAccessControlEntry(source);
	}

	public DocumentAccessControlEntry(long groupId) {
		super(groupId);
		setGroupId(groupId);
	}

	@Override
	public ExtendedAccessControlEntry getAce() {
		return ace;
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

	public void grantPermissions(Set<Permission> permissions) {
		ace.grantPermissions(permissions);
	}

	public Set<Permission> grantedPermissions() {
		return ace.grantedPermissions();
	}

	@Override
	public String toString() {
		return ace.toString();
	}

	@Override
	public boolean equals(Object obj) {
		return ace.equals(obj);
	}

	@Override
	public int hashCode() {
		return ace.hashCode();
	}
}