package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * LDAP Settings bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUILDAPServer implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private boolean enabled = false;

	private boolean anon = false;

	private boolean keepLocalMemberships = true;

	private String url;

	private String username;

	private String password;

	private String realm;

	private String userIdentifierAttr;

	private String groupIdentifierAttr;

	private String logonAttr;

	private String userClass;

	private String groupClass;

	private String userNodes;

	private String userIncludes;

	private String userExcludes;

	private String groupNodes;

	private String groupIncludes;

	private String groupExcludes;

	private String language;

	private int pageSize = 100;

	private int syncTtl = 0;

	private int userType = GUIUser.TYPE_DEFAULT;

	private int position = 1;

	private int timeout = 10;

	private String validation;

	private GUIGroup[] defaultGroups = new GUIGroup[0];

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
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

	public String getUserIncludes() {
		return userIncludes;
	}

	public String getUserExcludes() {
		return userExcludes;
	}

	public String getGroupIncludes() {
		return groupIncludes;
	}

	public String getGroupExcludes() {
		return groupExcludes;
	}

	public void setUserIncludes(String userIncludes) {
		this.userIncludes = userIncludes;
	}

	public void setUserExcludes(String userExcludes) {
		this.userExcludes = userExcludes;
	}

	public void setGroupIncludes(String groupIncludes) {
		this.groupIncludes = groupIncludes;
	}

	public void setGroupExcludes(String groupExcludes) {
		this.groupExcludes = groupExcludes;
	}

	public int getUserType() {
		return userType;
	}

	public void setUserType(int userType) {
		this.userType = userType;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
	
	public String getUserNodes() {
		return userNodes;
	}

	public String getGroupNodes() {
		return groupNodes;
	}

	public void setUserNodes(String userNodes) {
		this.userNodes = userNodes;
	}

	public void setGroupNodes(String groupNodes) {
		this.groupNodes = groupNodes;
	}

	public String getGroupIdentifierAttr() {
		return groupIdentifierAttr;
	}

	public String getGroupClass() {
		return groupClass;
	}

	public void setGroupIdentifierAttr(String groupIdentifierAttr) {
		this.groupIdentifierAttr = groupIdentifierAttr;
	}

	public void setGroupClass(String groupClass) {
		this.groupClass = groupClass;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public int getSyncTtl() {
		return syncTtl;
	}

	public void setSyncTtl(int syncTtl) {
		this.syncTtl = syncTtl;
	}

	public boolean isKeepLocalMemberships() {
		return keepLocalMemberships;
	}

	public void setKeepLocalMemberships(boolean keepLocalMemberships) {
		this.keepLocalMemberships = keepLocalMemberships;
	}

	public String getValidation() {
		return validation;
	}

	public void setValidation(String validation) {
		this.validation = validation;
	}

	public int getTimeout() {
		return timeout;
	}

	public void setTimeout(int timeout) {
		this.timeout = timeout;
	}

	public GUIGroup[] getDefaultGroups() {
		return defaultGroups;
	}

	public void setDefaultGroups(GUIGroup[] defaultGroups) {
		this.defaultGroups = defaultGroups;
	}
}