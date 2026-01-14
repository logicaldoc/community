package com.logicaldoc.webservice.model;

import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Useful class to associate a user or a group to a permission representation.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@XmlType(name = "WSAccessControlEntry")
public class WSAccessControlEntry {

	@WSDoc(description = "unique identifier of a group")
	private long groupId;

	@WSDoc(description = "unique identifier of a user")
	private long userId;

	@WSDoc(description = "permission to read")
	private boolean read = true;

	@WSDoc(description = "permission to preview")
	private boolean preview = true;

	@WSDoc(description = "permission to edit")
	private boolean write = false;

	@WSDoc(description = "permission to download")
	private boolean download = true;

	@WSDoc(description = "permission to add child elements")
	private boolean add = false;

	@WSDoc(description = "permission to change security policies")
	private boolean security = false;

	@WSDoc(description = "permission to delete")
	private boolean delete = false;

	@WSDoc(description = "permission to rename")
	private boolean rename = false;

	@WSDoc(description = "permission to mark a document as immutable")
	private boolean immutable = false;

	@WSDoc(description = "permission to import")
	private boolean iimport = false;

	@WSDoc(description = "permission to export")
	private boolean export = false;

	@WSDoc(description = "permission to digitally sign")
	private boolean sign = false;

	@WSDoc(description = "permission to archive")
	private boolean archive = false;

	@WSDoc(description = "permission to handle the workflow")
	private boolean workflow = false;

	@WSDoc(description = "permission to handle calendar events")
	private boolean calendar = false;

	@WSDoc(description = "permission to handle events subscription")
	private boolean subscription = false;

	@WSDoc(description = "permission to put a password")
	private boolean password = false;

	@WSDoc(description = "permission to prboolean")
	private boolean prboolean = true;

	@WSDoc(description = "permission to move")
	private boolean move = false;

	@WSDoc(description = "permission to send emails")
	private boolean email = true;

	@WSDoc(description = "permission to handle the automation")
	private boolean automation = false;

	@WSDoc(description = "permission to handle the store")
	private boolean store = false;

	@WSDoc(description = "permission to send reading requests")
	private boolean readingreq = false;

	@WSDoc(description = "permission to edit the Custom ID")
	private boolean customid = false;

	@WSDoc(description = "permission to edit the Revision")
	private boolean revision = false;

	public long getGroupId() {
		return groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public boolean isRead() {
		return read;
	}

	public void setRead(boolean read) {
		this.read = read;
	}

	public boolean isPreview() {
		return preview;
	}

	public void setPreview(boolean preview) {
		this.preview = preview;
	}

	public boolean isWrite() {
		return write;
	}

	public void setWrite(boolean write) {
		this.write = write;
	}

	public boolean isDownload() {
		return download;
	}

	public void setDownload(boolean download) {
		this.download = download;
	}

	public boolean isAdd() {
		return add;
	}

	public void setAdd(boolean add) {
		this.add = add;
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

	public boolean isPrboolean() {
		return prboolean;
	}

	public void setPrboolean(boolean prboolean) {
		this.prboolean = prboolean;
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

	public boolean isStore() {
		return store;
	}

	public void setStore(boolean store) {
		this.store = store;
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
}