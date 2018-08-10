package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.Constants;

/**
 * Workflow State bean as used in the GUI
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIWFState implements Serializable {

	private static final long serialVersionUID = 1L;

	public final static int TYPE_UNDEFINED = -1;

	public final static int TYPE_TASK = 0;

	public final static int TYPE_END = 1;

	public final static int TYPE_JOIN = 2;

	public final static int TYPE_FORK = 3;

	private int type = TYPE_TASK;

	private String id;

	private String name;

	private String description;

	private int dueDateNumber = 0;

	private String dueDateUnit = Constants.TIME_MINUTE;

	private int reminderNumber = 0;

	private String reminderUnit = Constants.TIME_MINUTE;

	private GUIValue[] participants;

	private GUITransition[] transitions;

	private String owner = "";

	private String pooledActors = "";

	private Date startDate;

	private Date endDate;

	private Date dueDate;

	private String taskState;

	private int top = 0;

	private int left = 0;

	private boolean initial = false;

	private String onCreation;

	private String onAssignment;

	private String display;

	private String color;

	public GUIWFState() {
	}

	public GUIWFState(String id, String name, int type) {
		this.id = id;
		this.name = name;
		this.type = type;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
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

	public GUIValue[] getParticipants() {
		return participants;
	}

	public void setParticipants(GUIValue[] participants) {
		this.participants = participants;
	}

	public GUITransition[] getTransitions() {
		return transitions;
	}

	public void setTransitions(GUITransition[] transitions) {
		this.transitions = transitions;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getIcon() {
		if (type == TYPE_TASK) {
			return "[SKIN]/task.png";
		} else if (type == TYPE_JOIN) {
			return "[SKIN]/join.png";
		} else if (type == TYPE_FORK) {
			return "[SKIN]/fork.png";
		} else if (type == TYPE_END) {
			return "[SKIN]/endState.png";
		} else {
			return "";
		}
	}

	public int getDueDateNumber() {
		return dueDateNumber;
	}

	public void setDueDateNumber(int dueDateNumber) {
		this.dueDateNumber = dueDateNumber;
	}

	public String getDueDateUnit() {
		return dueDateUnit;
	}

	public void setDueDateUnit(String dueDateUnit) {
		this.dueDateUnit = dueDateUnit;
	}

	public int getReminderNumber() {
		return reminderNumber;
	}

	public void setReminderNumber(int reminderNumber) {
		this.reminderNumber = reminderNumber;
	}

	public String getReminderUnit() {
		return reminderUnit;
	}

	public void setReminderUnit(String reminderUnit) {
		this.reminderUnit = reminderUnit;
	}

	public String getOwner() {
		return owner;
	}

	public void setOwner(String owner) {
		this.owner = owner;
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

	public String getTaskState() {
		return taskState;
	}

	public void setTaskState(String taskState) {
		this.taskState = taskState;
	}

	public String getPooledActors() {
		return pooledActors;
	}

	public void setPooledActors(String pooledActors) {
		this.pooledActors = pooledActors;
	}

	public int getTop() {
		return top;
	}

	public void setTop(int top) {
		this.top = top;
	}

	public int getLeft() {
		return left;
	}

	public void setLeft(int left) {
		this.left = left;
	}

	public boolean isInitial() {
		return initial;
	}

	public void setInitial(boolean initial) {
		this.initial = initial;
	}

	public Date getDueDate() {
		return dueDate;
	}

	public void setDueDate(Date dueDate) {
		this.dueDate = dueDate;
	}

	public String getOnCreation() {
		return onCreation;
	}

	public void setOnCreation(String onCreation) {
		this.onCreation = onCreation;
	}

	public String getOnAssignment() {
		return onAssignment;
	}

	public void setOnAssignment(String onAssignment) {
		this.onAssignment = onAssignment;
	}

	public String getDisplay() {
		return display;
	}

	public void setDisplay(String display) {
		this.display = display;
	}
	
	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}
}