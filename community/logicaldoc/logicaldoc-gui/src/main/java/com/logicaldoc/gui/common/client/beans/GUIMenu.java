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

	private String name;

	private GUIRight[] rights = new GUIRight[] {};

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
}