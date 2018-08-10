package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * LDAP Settings bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUILdapSettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private String implementation = "";

	private boolean enabled = false;

	private boolean anon = false;

	private String url;

	private String username;

	private String pwd;

	private String realm;

	private String userIdentifierAttr;

	private String grpIdentifierAttr;

	private String logonAttr;

	private String userClass;

	private String grpClass;

	private String usersBaseNode;

	private String userInclude;

	private String userExclude;

	private String grpsBaseNode;

	private String groupInclude;

	private String groupExclude;

	private String language;

	private int pageSize = 100;

	public String getImplementation() {
		return implementation;
	}

	public void setImplementation(String implementation) {
		this.implementation = implementation;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPwd() {
		return pwd;
	}

	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	public String getRealm() {
		return realm;
	}

	public void setRealm(String realm) {
		this.realm = realm;
	}

	public String getUserIdentifierAttr() {
		return userIdentifierAttr;
	}

	public void setUserIdentifierAttr(String userIdentifierAttr) {
		this.userIdentifierAttr = userIdentifierAttr;
	}

	public String getGroupIdentifierAttr() {
		return grpIdentifierAttr;
	}

	public void setGroupIdentifierAttr(String grpIdentifierAttr) {
		this.grpIdentifierAttr = grpIdentifierAttr;
	}

	public String getLogonAttr() {
		return logonAttr;
	}

	public void setLogonAttr(String logonAttr) {
		this.logonAttr = logonAttr;
	}

	public String getUserClass() {
		return userClass;
	}

	public void setUserClass(String userClass) {
		this.userClass = userClass;
	}

	public String getGrpClass() {
		return grpClass;
	}

	public void setGrpClass(String grpClass) {
		this.grpClass = grpClass;
	}

	public String getUsersBaseNode() {
		return usersBaseNode;
	}

	public void setUsersBaseNode(String usersBaseNode) {
		this.usersBaseNode = usersBaseNode;
	}

	public String getGroupsBaseNode() {
		return grpsBaseNode;
	}

	public void setGroupsBaseNode(String grpsBaseNode) {
		this.grpsBaseNode = grpsBaseNode;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public int getPageSize() {
		return pageSize;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public boolean isAnonymous() {
		return anon;
	}

	public void setAnonymous(boolean anon) {
		this.anon = anon;
	}

	public String getUserInclude() {
		return userInclude;
	}

	public void setUserInclude(String userInclude) {
		this.userInclude = userInclude;
	}

	public String getUserExclude() {
		return userExclude;
	}

	public void setUserExclude(String userExclude) {
		this.userExclude = userExclude;
	}

	public String getGroupInclude() {
		return groupInclude;
	}

	public void setGroupInclude(String groupInclude) {
		this.groupInclude = groupInclude;
	}

	public String getGroupExclude() {
		return groupExclude;
	}

	public void setGroupExclude(String groupExclude) {
		this.groupExclude = groupExclude;
	}
}