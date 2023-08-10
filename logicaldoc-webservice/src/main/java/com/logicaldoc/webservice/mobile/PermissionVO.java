package com.logicaldoc.webservice.mobile;

import java.io.Serializable;

public class PermissionVO implements Serializable {

	private static final long serialVersionUID = 1L;

	private boolean create = false;

	private boolean edit = false;

	private boolean delete = false;

	public boolean isCreate() {
		return create;
	}

	public void setCreate(boolean create) {
		this.create = create;
	}

	public boolean isEdit() {
		return edit;
	}

	public void setEdit(boolean edit) {
		this.edit = edit;
	}

	public boolean isDelete() {
		return delete;
	}

	public void setDelete(boolean delete) {
		this.delete = delete;
	}
}