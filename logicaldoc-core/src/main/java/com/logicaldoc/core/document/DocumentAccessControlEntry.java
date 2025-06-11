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
	private ExtendedAccessControlEntry extendedAccessControlEntry = new ExtendedAccessControlEntry();

	public long getGroupId() {
		return extendedAccessControlEntry.getGroupId();
	}

	public int getWrite() {
		return extendedAccessControlEntry.getWrite();
	}

	public void setGroupId(long groupId) {
		extendedAccessControlEntry.setGroupId(groupId);
	}

	public void setWrite(int write) {
		extendedAccessControlEntry.setWrite(write);
	}

	public int getRead() {
		return extendedAccessControlEntry.getRead();
	}

	public void setRead(int read) {
		extendedAccessControlEntry.setRead(read);
	}

	public Set<Permission> grantedPermissions() {
		return extendedAccessControlEntry.grantedPermissions();
	}

	public boolean equals(Object obj) {
		return extendedAccessControlEntry.equals(obj);
	}

	public int hashCode() {
		return extendedAccessControlEntry.hashCode();
	}

	public void grantPermissions(Set<Permission> permissions) {
		extendedAccessControlEntry.grantPermissions(permissions);
	}

	public int getPreview() {
		return extendedAccessControlEntry.getPreview();
	}

	public void setPreview(int preview) {
		extendedAccessControlEntry.setPreview(preview);
	}

	public int getDownload() {
		return extendedAccessControlEntry.getDownload();
	}

	public void setDownload(int download) {
		extendedAccessControlEntry.setDownload(download);
	}

	public int getSecurity() {
		return extendedAccessControlEntry.getSecurity();
	}

	public void setSecurity(int security) {
		extendedAccessControlEntry.setSecurity(security);
	}

	public int getDelete() {
		return extendedAccessControlEntry.getDelete();
	}

	public void setDelete(int delete) {
		extendedAccessControlEntry.setDelete(delete);
	}

	public int getRename() {
		return extendedAccessControlEntry.getRename();
	}

	public void setRename(int rename) {
		extendedAccessControlEntry.setRename(rename);
	}

	public int getImmutable() {
		return extendedAccessControlEntry.getImmutable();
	}

	public void setImmutable(int immutable) {
		extendedAccessControlEntry.setImmutable(immutable);
	}

	public int getSign() {
		return extendedAccessControlEntry.getSign();
	}

	public void setSign(int sign) {
		extendedAccessControlEntry.setSign(sign);
	}

	public int getArchive() {
		return extendedAccessControlEntry.getArchive();
	}

	public void setArchive(int archive) {
		extendedAccessControlEntry.setArchive(archive);
	}

	public int getWorkflow() {
		return extendedAccessControlEntry.getWorkflow();
	}

	public void setWorkflow(int workflow) {
		extendedAccessControlEntry.setWorkflow(workflow);
	}

	public int getCalendar() {
		return extendedAccessControlEntry.getCalendar();
	}

	public void setCalendar(int calendar) {
		extendedAccessControlEntry.setCalendar(calendar);
	}

	public int getSubscription() {
		return extendedAccessControlEntry.getSubscription();
	}

	public void setSubscription(int subscription) {
		extendedAccessControlEntry.setSubscription(subscription);
	}

	public int getPassword() {
		return extendedAccessControlEntry.getPassword();
	}

	public void setPassword(int password) {
		extendedAccessControlEntry.setPassword(password);
	}

	public int getPrint() {
		return extendedAccessControlEntry.getPrint();
	}

	public void setPrint(int print) {
		extendedAccessControlEntry.setPrint(print);
	}

	public int getMove() {
		return extendedAccessControlEntry.getMove();
	}

	public void setMove(int move) {
		extendedAccessControlEntry.setMove(move);
	}

	public int getEmail() {
		return extendedAccessControlEntry.getEmail();
	}

	public void setEmail(int email) {
		extendedAccessControlEntry.setEmail(email);
	}

	public int getAutomation() {
		return extendedAccessControlEntry.getAutomation();
	}

	public void setAutomation(int automation) {
		extendedAccessControlEntry.setAutomation(automation);
	}

	public int getReadingreq() {
		return extendedAccessControlEntry.getReadingreq();
	}

	public void setReadingreq(int readingreq) {
		extendedAccessControlEntry.setReadingreq(readingreq);
	}

	public int getCustomid() {
		return extendedAccessControlEntry.getCustomid();
	}

	public void setCustomid(int customid) {
		extendedAccessControlEntry.setCustomid(customid);
	}

	public String toString() {
		return extendedAccessControlEntry.toString();
	}

	public DocumentAccessControlEntry() {
	}

	public DocumentAccessControlEntry(DocumentAccessControlEntry source) {
		super(source);
		extendedAccessControlEntry=new ExtendedAccessControlEntry(source);
	}

	public DocumentAccessControlEntry(long groupId) {
		super(groupId);
	    setGroupId(groupId);
	}
	
	
}
