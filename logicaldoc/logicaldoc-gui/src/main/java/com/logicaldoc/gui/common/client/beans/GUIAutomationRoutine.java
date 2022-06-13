package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

public class GUIAutomationRoutine extends GUIExtensibleObject implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name;

	private String description;

	private String automation;

	public GUIAutomationRoutine(long id) {
		super();
		this.id = id;
	}

	public GUIAutomationRoutine() {

	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
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

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}
}