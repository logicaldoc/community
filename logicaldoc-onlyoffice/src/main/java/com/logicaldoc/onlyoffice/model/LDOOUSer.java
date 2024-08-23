package com.logicaldoc.onlyoffice.model;

import com.onlyoffice.model.common.User;

public class LDOOUSer extends User {

	private String sid;
	private String email;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public LDOOUSer() {
	}

	public LDOOUSer(String id, String name, String group, String image, String email, String sid) {
		super(id, name, group, image);
		this.email = email;
		this.sid= sid;
	}

}
