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
		setGroupId(source.getGroupId());
		setRead(source.isRead());
		setWrite(source.isWrite());

		setArchive(source.isArchive());
		setAutomation(source.isAutomation());
		setCalendar(source.isCalendar());
		setDelete(source.isDelete());
		setDownload(source.isDownload());
		setEmail(source.isEmail());
		setImmutable(source.isImmutable());
		setMove(source.isMove());
		setMove(source.isMove());
		setPassword(source.isPassword());
		setPreview(source.isPreview());
		setPrint(source.isPrint());
		setPrint(source.isPrint());
		setRename(source.isRename());
		setSecurity(source.isSecurity());
		setSign(source.isSign());
		setSubscription(source.isSubscription());
		setWorkflow(source.isWorkflow());
		setReadingreq(source.isReadingreq());
		setCustomid(source.isCustomid());
		setRevision(source.isRevision());
	}

	public DocumentAccessControlEntry(long groupId) {
		super(groupId);
		setGroupId(groupId);
	}

	@Override
	public ExtendedAccessControlEntry getAce() {
		return ace;
	}

	@Override
	public void setGroupId(long groupId) {
		ace.setGroupId(groupId);
	}

	@Override
	public long getGroupId() {
		return ace.getGroupId();
	}

	@Override
	public boolean isRead() {
		return ace.isRead();
	}

	@Override
	public void setRead(boolean read) {
		ace.setRead(read);
	}

	@Override
	public boolean isWrite() {
		return ace.isWrite();
	}

	@Override
	public void setWrite(boolean write) {
		ace.setWrite(write);
	}

	@Override
	public boolean isPreview() {
		return ace.isPreview();
	}

	@Override
	public void setPreview(boolean preview) {
		ace.setPreview(preview);
	}

	@Override
	public boolean isDownload() {
		return ace.isDownload();
	}

	@Override
	public void setDownload(boolean download) {
		ace.setDownload(download);
	}

	@Override
	public boolean isSecurity() {
		return ace.isSecurity();
	}

	@Override
	public void setSecurity(boolean security) {
		ace.setSecurity(security);
	}

	@Override
	public boolean isDelete() {
		return ace.isDelete();
	}

	@Override
	public void setDelete(boolean delete) {
		ace.setDelete(delete);
	}

	@Override
	public boolean isRename() {
		return ace.isRename();
	}

	@Override
	public void setRename(boolean rename) {
		ace.setRename(rename);
	}

	@Override
	public boolean isImmutable() {
		return ace.isImmutable();
	}

	@Override
	public void setImmutable(boolean immutable) {
		ace.setImmutable(immutable);
	}

	@Override
	public boolean isSign() {
		return ace.isSign();
	}

	@Override
	public void setSign(boolean sign) {
		ace.setSign(sign);
	}

	@Override
	public boolean isArchive() {
		return ace.isArchive();
	}

	@Override
	public void setArchive(boolean archive) {
		ace.setArchive(archive);
	}

	@Override
	public boolean isWorkflow() {
		return ace.isWorkflow();
	}

	@Override
	public void setWorkflow(boolean workflow) {
		ace.setWorkflow(workflow);
	}

	@Override
	public boolean isCalendar() {
		return ace.isCalendar();
	}

	@Override
	public void setCalendar(boolean calendar) {
		ace.setCalendar(calendar);
	}

	@Override
	public boolean isSubscription() {
		return ace.isSubscription();
	}

	@Override
	public void setSubscription(boolean subscription) {
		ace.setSubscription(subscription);
	}

	@Override
	public boolean isPassword() {
		return ace.isPassword();
	}

	@Override
	public void setPassword(boolean password) {
		ace.setPassword(password);
	}

	@Override
	public boolean isPrint() {
		return ace.isPrint();
	}

	@Override
	public void setPrint(boolean print) {
		ace.setPrint(print);
	}

	@Override
	public boolean isMove() {
		return ace.isMove();
	}

	@Override
	public void setMove(boolean move) {
		ace.setMove(move);
	}

	@Override
	public boolean isEmail() {
		return ace.isEmail();
	}

	@Override
	public void setEmail(boolean email) {
		ace.setEmail(email);
	}

	@Override
	public boolean isAutomation() {
		return ace.isAutomation();
	}

	@Override
	public void setAutomation(boolean automation) {
		ace.setAutomation(automation);
	}

	@Override
	public boolean isReadingreq() {
		return ace.isReadingreq();
	}

	@Override
	public void setReadingreq(boolean readingreq) {
		ace.setReadingreq(readingreq);
	}

	@Override
	public boolean isCustomid() {
		return ace.isCustomid();
	}

	@Override
	public void setCustomid(boolean customid) {
		ace.setCustomid(customid);
	}

	@Override
	public boolean isRevision() {
		return ace.isRevision();
	}

	@Override
	public void setRevision(boolean revision) {
		ace.setRevision(revision);
	}

	@Override
	public void grantPermissions(Set<Permission> permissions) {
		ace.grantPermissions(permissions);
	}

	@Override
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