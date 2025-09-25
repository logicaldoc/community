package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;

/**
 * Defins the rules to automatically trigger a routine.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class GUIAutomationTrigger implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String events;

	private GUIFolder folder;

	/**
	 * Name of the declared routine to execute
	 */
	private GUIAutomationRoutine routine;

	/**
	 * Automation script to execute(in absence of routine specification)
	 */
	private String automation;

	/**
	 * For a time-driven trigger, the exact moment to fire the execution
	 */
	private Date date;

	/**
	 * For a time-driven trigger, the cron expression to fire the execution
	 */
	private String cron;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getEvents() {
		return events;
	}

	public String[] getEventsArray() {
		if (events != null && !events.isEmpty()) {
			if (!events.contains(","))
				return new String[] { events };
			else {
				ArrayList<String> buf = new ArrayList<>();
				String[] tokens = events.split(",");
				for (String token : tokens) {
					if (!token.isEmpty())
						buf.add(token);
				}
				return buf.toArray(new String[0]);
			}
		} else
			return new String[0];
	}

	public void setEvents(String events) {
		this.events = events;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public GUIAutomationRoutine getRoutine() {
		return routine;
	}

	public void setRoutine(GUIAutomationRoutine routine) {
		this.routine = routine;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getCron() {
		return cron;
	}

	public void setCron(String cron) {
		this.cron = cron;
	}
}