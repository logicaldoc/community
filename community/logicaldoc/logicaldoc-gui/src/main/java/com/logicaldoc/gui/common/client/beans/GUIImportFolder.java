package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * An Import Folder representation
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class GUIImportFolder implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String path;

	private String domain;

	private String username;

	private String password;

	private String provider = "smb";

	private GUIFolder target = null;

	private int enabled = 1;

	private Integer maxSize = null;

	private int extractTags = 0;

	private String includes = "*.*";

	private String excludes = "~*,*~,#*#,.#*,%*%,._*,.DS_Store,desktop.ini,Thumbs";

	private String tags = "";

	private String language = "";

	private Long templateId;

	private int depth = 5;

	private boolean delImport = false;

	private boolean importEmpty = false;

	private Date startDate;

	private int updatePolicy = 0;
	
	private boolean inheritRights = true;

	public GUIImportFolder() {
		super();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getProvider() {
		return provider;
	}

	public void setProvider(String provider) {
		this.provider = provider;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public Integer getMaxSize() {
		return maxSize;
	}

	public void setMaxSize(Integer maxSize) {
		this.maxSize = maxSize;
	}

	public int getExtractTags() {
		return extractTags;
	}

	public void setExtractTags(int extractTags) {
		this.extractTags = extractTags;
	}

	public String getIncludes() {
		return includes;
	}

	public void setIncludes(String includes) {
		this.includes = includes;
	}

	public String getExcludes() {
		return excludes;
	}

	public void setExcludes(String excludes) {
		this.excludes = excludes;
	}

	public String getTags() {
		return tags;
	}

	public void setTags(String tags) {
		this.tags = tags;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public int getDepth() {
		return depth;
	}

	public void setDepth(int depth) {
		this.depth = depth;
	}

	public boolean isDelImport() {
		return delImport;
	}

	public void setDelImport(boolean delImport) {
		this.delImport = delImport;
	}

	public GUIFolder getTarget() {
		return target;
	}

	public void setTarget(GUIFolder target) {
		this.target = target;
	}

	public String getDomain() {
		return domain;
	}

	public void setDomain(String domain) {
		this.domain = domain;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public int getUpdatePolicy() {
		return updatePolicy;
	}

	public void setUpdatePolicy(int updatePolicy) {
		this.updatePolicy = updatePolicy;
	}

	public boolean isImportEmpty() {
		return importEmpty;
	}

	public void setImportEmpty(boolean importEmpty) {
		this.importEmpty = importEmpty;
	}

	public boolean isInheritRights() {
		return inheritRights;
	}

	public void setInheritRights(boolean inheritRights) {
		this.inheritRights = inheritRights;
	}
}