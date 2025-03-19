package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * A GUI bean representing an AI model
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIModel implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private String type = "neural";

	public GUIModel(long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public GUIModel() {
		super();
	}

	public long getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}

	public String getDescription() {
		return description;
	}

	public String getType() {
		return type;
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setType(String type) {
		this.type = type;
	}
}