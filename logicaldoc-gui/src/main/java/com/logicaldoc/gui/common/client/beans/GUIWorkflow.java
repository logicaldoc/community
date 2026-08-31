package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Workflow bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIWorkflow implements Serializable {

	private static final long serialVersionUID = 1L;

	private String id = null;

	private String name = "";

	private String label = "";

	private String color = null;

	private String description = "";

	private String tag = "";

	private String startStateId = null;

	private GUIWFState selectedTask = null;

	private Date startDate;

	private Date endDate;

	private String appendedDocIds;

	// The persistence template
	private Long templateId;

	private int version = 1;

	private Date date = new Date();

	private boolean latestVersion = true;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	private List<String> permissions = new ArrayList<>();

	private List<GUIUser> supervisors = new ArrayList<>();

	private List<GUIWFState> states = new ArrayList<>();

	private List<GUIDocument> appendedDocs = new ArrayList<>();

	private List<GUIWFState> wflHistory = new ArrayList<>();

	public GUIWFState getStateById(String id) {
		for (GUIWFState state : states) {
			if (state.getId().trim().equals(id.trim())) {
				return state;
			}
		}
		return null;
	}

	public GUIWFState getStateByName(String name) {
		for (GUIWFState state : states) {
			if (state.getName().trim().equals(name.trim())) {
				return state;
			}
		}
		return null;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
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

	public String getStartStateId() {
		return startStateId;
	}

	public void setStartStateId(String startStateId) {
		this.startStateId = startStateId;
	}

	public GUIWFState getSelectedTask() {
		return selectedTask;
	}

	public void setSelectedTask(GUIWFState selectedTask) {
		this.selectedTask = selectedTask;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public String getAppendedDocIds() {
		return appendedDocIds;
	}

	public void setAppendedDocIds(String appendedDocIds) {
		this.appendedDocIds = appendedDocIds;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public String getTag() {
		return tag;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	public int getVersion() {
		return version;
	}

	public void setVersion(int version) {
		this.version = version;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public boolean isLatestVersion() {
		return latestVersion;
	}

	public void setLatestVersion(boolean latestVersion) {
		this.latestVersion = latestVersion;
	}

	public boolean isWrite() {
		return hasPermission(GUIAccessControlEntry.PERMISSION_WRITE);
	}

	public boolean hasPermission(String permission) {
		if (permissions == null)
			return false;
		for (String p : permissions)
			if (p.equals(permission))
				return true;
		return false;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public List<String> getPermissions() {
		return permissions;
	}

	public void setPermissions(List<String> permissions) {
		this.permissions = permissions;
	}

	public List<GUIUser> getSupervisors() {
		return supervisors;
	}

	public void setSupervisors(List<GUIUser> supervisors) {
		this.supervisors = supervisors;
	}

	public List<GUIWFState> getStates() {
		return states;
	}

	public void setStates(List<GUIWFState> states) {
		this.states = states;
	}

	public List<GUIDocument> getAppendedDocs() {
		return appendedDocs;
	}

	public void setAppendedDocs(List<GUIDocument> appendedDocs) {
		this.appendedDocs = appendedDocs;
	}

	public List<GUIWFState> getWflHistory() {
		return wflHistory;
	}

	public void setWflHistory(List<GUIWFState> wflHistory) {
		this.wflHistory = wflHistory;
	}
}