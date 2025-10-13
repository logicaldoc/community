package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * A Report representation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class GUIReport implements Serializable {

	public static final int STATUS_IDLE = 0;

	public static final int STATUS_RUNNING = 1;

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private GUIFolder outputFolder = null;

	private int enabled = 1;

	private String name;

	private String description;

	private int status = STATUS_IDLE;

	private Long outputDocId = null;

	private String outputFormat = "html";

	private int updatePolicy = 0;

	private Date lastRun;

	private Date lastModified;

	private long recordVersion = 0;

	private String log;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	/**
	 * Permissions granted to the current user on this report
	 */
	private GUIAccessControlEntry allowedPermissions = new GUIAccessControlEntry();

	public GUIReport() {
		super();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public GUIFolder getOutputFolder() {
		return outputFolder;
	}

	public void setOutputFolder(GUIFolder outputFolder) {
		this.outputFolder = outputFolder;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public Long getOutputDocId() {
		return outputDocId;
	}

	public void setOutputDocId(Long outputDocId) {
		this.outputDocId = outputDocId;
	}

	public String getOutputFormat() {
		return outputFormat;
	}

	public void setOutputFormat(String outputFormat) {
		this.outputFormat = outputFormat;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getUpdatePolicy() {
		return updatePolicy;
	}

	public void setUpdatePolicy(int updatePolicy) {
		this.updatePolicy = updatePolicy;
	}

	public Date getLastRun() {
		return lastRun;
	}

	public void setLastRun(Date lastRun) {
		this.lastRun = lastRun;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	public long getRecordVersion() {
		return recordVersion;
	}

	public void setRecordVersion(long recordVersion) {
		this.recordVersion = recordVersion;
	}

	public String getLog() {
		return log;
	}

	public void setLog(String log) {
		this.log = log;
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public GUIAccessControlEntry getAllowedPermissions() {
		return allowedPermissions;
	}

	public void setAllowedPermissions(GUIAccessControlEntry allowedPermissions) {
		this.allowedPermissions = allowedPermissions;
	}

	public boolean isRead() {
		return allowedPermissions.isRead();
	}

	public boolean isWrite() {
		return allowedPermissions.isWrite();
	}
}