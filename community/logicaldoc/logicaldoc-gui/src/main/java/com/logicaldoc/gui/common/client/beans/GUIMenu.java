package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Represents a Menu from the GUI view
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIMenu implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private long parentId;

	private String name;

	private String description;

	private boolean enabled = true;

	/**
	 * The declared routine to execute
	 */
	private Long routineId;

	/**
	 * Automation script to execute(in absence of routine specification)
	 */
	private String automation;

	private int position = 0;

	private GUIRight[] rights = new GUIRight[] {};

	private Long securityRef;

	private int type = 0;

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

	public GUIRight[] getRights() {
		return rights;
	}

	public void setRights(GUIRight[] rights) {
		this.rights = rights;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public Long getRoutineId() {
		return routineId;
	}

	public void setRoutineId(Long routineId) {
		this.routineId = routineId;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public long getParentId() {
		return parentId;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public Long getSecurityRef() {
		return securityRef;
	}

	public void setSecurityRef(Long securityRef) {
		this.securityRef = securityRef;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}
}