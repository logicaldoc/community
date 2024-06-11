package com.logicaldoc.webservice.model;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

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
	private int read = 1;
	
	@WSDoc(description = "permission to preview")
	private int preview = 1;

	@WSDoc(description = "permission to edit")
	private int write = 0;

	@WSDoc(description = "permission to download")
	private int download = 1;

	@WSDoc(description = "permission to add child elements")
	private int add = 0;

	@WSDoc(description = "permission to change security policies")
	private int security = 0;

	@WSDoc(description = "permission to delete")
	private int delete = 0;

	@WSDoc(description = "permission to rename")
	private int rename = 0;

	@WSDoc(description = "permission to mark a document as immutable")
	private int immutable = 0;

	@WSDoc(description = "permission to import")
	private int iimport = 0;

	@WSDoc(description = "permission to export")
	private int export = 0;

	@WSDoc(description = "permission to digitally sign")
	private int sign = 0;

	@WSDoc(description = "permission to archive")
	private int archive = 0;

	@WSDoc(description = "permission to handle the workflow")
	private int workflow = 0;

	@WSDoc(description = "permission to handle calendar events")
	private int calendar = 0;

	@WSDoc(description = "permission to handle events subscription")
	private int subscription = 0;

	@WSDoc(description = "permission to put a password")
	private int password = 0;

	@WSDoc(description = "permission to print")
	private int print = 1;

	@WSDoc(description = "permission to move")
	private int move = 0;

	@WSDoc(description = "permission to send emails")
	private int email = 1;

	@WSDoc(description = "permission to handle the automation")
	private int automation = 0;

	@WSDoc(description = "permission to handle the storage")
	private int storage = 0;

	@WSDoc(description = "permission to send reading requests")
	private int readingreq = 0;

	@WSDoc(description = "permission to edit the Custom ID")
	private int customid = 0;
	
	public long getGroupId() {
		return groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	public int getRead() {
		return read;
	}

	public void setRead(int read) {
		this.read = read;
	}

	public int getWrite() {
		return write;
	}

	public void setWrite(int write) {
		this.write = write;
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

	public int getIimport() {
		return iimport;
	}

	public void setIimport(int iimport) {
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

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public int getPreview() {
		return preview;
	}

	public void setPreview(int preview) {
		this.preview = preview;
	}

	public int getCustomid() {
		return customid;
	}

	public void setCustomid(int customid) {
		this.customid = customid;
	}
}