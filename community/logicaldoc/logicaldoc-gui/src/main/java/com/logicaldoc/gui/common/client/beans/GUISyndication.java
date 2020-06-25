package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * A Syndication representation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class GUISyndication implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name;

	private String description;

	private String url;

	private String username;

	private String password;

	private int enabled = 1;

	private GUIFolder sourceFolder;

	private String targetPath;

	private String includes;

	private String excludes;

	private long maxPacketSize = 1024;

	private long batch = 1000;
	
	private Date startDate = null;
	
	private int replicateCustomId = 1;
	
	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public long getBatch() {
		return batch;
	}

	public void setBatch(long batch) {
		this.batch = batch;
	}

	public long getMaxPacketSize() {
		return maxPacketSize;
	}

	public void setMaxPacketSize(long maxPacketSize) {
		this.maxPacketSize = maxPacketSize;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public GUIFolder getSourceFolder() {
		return sourceFolder;
	}

	public void setSourceFolder(GUIFolder sourceFolder) {
		this.sourceFolder = sourceFolder;
	}

	public String getTargetPath() {
		return targetPath;
	}

	public void setTargetPath(String targetPath) {
		this.targetPath = targetPath;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
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

	public int getReplicateCustomId() {
		return replicateCustomId;
	}

	public void setReplicateCustomId(int replicateCustomId) {
		this.replicateCustomId = replicateCustomId;
	}
}