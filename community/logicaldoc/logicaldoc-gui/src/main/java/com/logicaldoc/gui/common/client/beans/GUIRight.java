package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Bean for rights assignments
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIRight implements Serializable {

	private static final long serialVersionUID = 1L;

	private long entityId = 0;

	private boolean read = true;

	private boolean print = true;

	private boolean write = false;

	private boolean delete = false;

	private boolean add = false;

	private boolean _import = false;

	private boolean workflow = false;

	private boolean sign = false;

	private boolean export = false;

	private boolean immutable = false;

	private boolean rename = false;

	private boolean security = false;

	private boolean archive = false;

	private boolean download = true;

	private boolean calendar = false;

	private boolean subscription = false;

	private boolean password = false;

	private boolean move = false;

	private boolean email = false;

	private boolean automation = false;
	
	private boolean storage = false;

	private String name;

	private String label;

	public long getEntityId() {
		return entityId;
	}

	public void setEntityId(long entityId) {
		this.entityId = entityId;
	}

	public boolean isRead() {
		return read;
	}

	public void setRead(boolean read) {
		this.read = read;
	}

	public boolean isWrite() {
		return write;
	}

	public void setWrite(boolean write) {
		this.write = write;
	}

	public boolean isDelete() {
		return delete;
	}

	public void setDelete(boolean delete) {
		this.delete = delete;
	}

	public boolean isAdd() {
		return add;
	}

	public void setAdd(boolean add) {
		this.add = add;
	}

	public boolean isImport() {
		return _import;
	}

	public void setImport(boolean _import) {
		this._import = _import;
	}

	public boolean isWorkflow() {
		return workflow;
	}

	public void setWorkflow(boolean workflow) {
		this.workflow = workflow;
	}

	public boolean isSign() {
		return sign;
	}

	public void setSign(boolean sign) {
		this.sign = sign;
	}

	public boolean isExport() {
		return export;
	}

	public void setExport(boolean export) {
		this.export = export;
	}

	public boolean isImmutable() {
		return immutable;
	}

	public void setImmutable(boolean immutable) {
		this.immutable = immutable;
	}

	public boolean isRename() {
		return rename;
	}

	public void setRename(boolean rename) {
		this.rename = rename;
	}

	public boolean isSecurity() {
		return security;
	}

	public void setSecurity(boolean security) {
		this.security = security;
	}

	public boolean isArchive() {
		return archive;
	}

	public void setArchive(boolean archive) {
		this.archive = archive;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public boolean isDownload() {
		return download;
	}

	public void setDownload(boolean download) {
		this.download = download;
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

	public boolean isPrint() {
		return print;
	}

	public void setPrint(boolean print) {
		this.print = print;
	}

	public boolean isPassword() {
		return password;
	}

	public void setPassword(boolean password) {
		this.password = password;
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

	public boolean isStorage() {
		return storage;
	}

	public void setStorage(boolean storage) {
		this.storage = storage;
	}

	@Override
	public String toString() {
		return getName();
	}
}