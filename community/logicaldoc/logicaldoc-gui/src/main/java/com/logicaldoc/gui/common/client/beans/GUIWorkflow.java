package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Workflow bean as used in the GUI
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class GUIWorkflow implements Serializable {

	private static final long serialVersionUID = 1L;

	private String id = null;

	private String name = "";

	private String description = "";
	
	private String tag = "";

	private String startStateId = null;

	private String supervisor = "";

	private GUIWFState[] states = new GUIWFState[0];

	private GUIWFState selectedTask = null;

	private Date startDate;

	private Date endDate;

	private GUIDocument[] appendedDocs;

	private String appendedDocIds;

	private GUIWFState[] wflHistory;
	
	//The persistence template
	private Long templateId;

	public GUIWFState getStateById(String id) {
		if (states != null && states.length > 0) {
			for (GUIWFState state : states) {
				if (state.getId().equals(id)) {
					return state;
				}
			}
		}
		return null;
	}

	public GUIWFState getStateByName(String name) {
		if (states != null && states.length > 0) {
			for (GUIWFState state : states) {
				if (state.getName().equals(name)) {
					return state;
				}
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

	public String getSupervisor() {
		return supervisor;
	}

	public void setSupervisor(String supervisor) {
		this.supervisor = supervisor;
	}

	public String getStartStateId() {
		return startStateId;
	}

	public void setStartStateId(String startStateId) {
		this.startStateId = startStateId;
	}

	public GUIWFState[] getStates() {
		return states;
	}

	public void setStates(GUIWFState[] states) {
		this.states = states;
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

	public GUIDocument[] getAppendedDocs() {
		return appendedDocs;
	}

	public void setAppendedDocs(GUIDocument[] appendedDocs) {
		this.appendedDocs = appendedDocs;
	}

	public GUIWFState[] getWflHistory() {
		return wflHistory;
	}

	public void setWflHistory(GUIWFState[] wflHistory) {
		this.wflHistory = wflHistory;
	}

	public String getAppendedDocIds() {
		return appendedDocIds;
	}

	public void setAppendedDocIds(String appendedDocIds) {
		this.appendedDocIds = appendedDocIds;
	}

	public void addState(GUIWFState status) {
		GUIWFState[] buf = new GUIWFState[getStates().length];
		for (int i = 0; i < getStates().length; i++) {
			buf[i] = getStates()[i];
		}
		buf[buf.length - 1] = status;
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
}
