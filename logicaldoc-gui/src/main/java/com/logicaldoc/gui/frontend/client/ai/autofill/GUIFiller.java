package com.logicaldoc.gui.frontend.client.ai.autofill;

import java.io.Serializable;

/**
 * A GUI bean representing an AI filler
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class GUIFiller implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private String type;

	private String model;

	private Long modelId;

	private Double threshold = 0.7d;

	public GUIFiller() {
		// Empty constructor
	}

	public GUIFiller(long id, String name) {
		super();
		this.id = id;
		this.name = name;
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

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Long getModelId() {
		return modelId;
	}

	public void setModelId(Long modelId) {
		this.modelId = modelId;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public Double getThreshold() {
		return threshold;
	}

	public void setThreshold(Double threshold) {
		this.threshold = threshold;
	}
}