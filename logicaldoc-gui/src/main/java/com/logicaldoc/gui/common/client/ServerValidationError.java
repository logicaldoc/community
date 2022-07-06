package com.logicaldoc.gui.common.client;

import java.io.Serializable;

/**
 * A generic validation error
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class ServerValidationError implements Serializable {

	private static final long serialVersionUID = 1L;

	private String attribute;

	private String label;

	private String description;

	public ServerValidationError() {
		super();
	}

	
	public ServerValidationError(String attribute, String label, String description) {
		super();
		this.attribute = attribute;
		this.label = label;
		this.description = description;
	}

	public String getAttribute() {
		return attribute;
	}

	public String getLabel() {
		return label;
	}

	public String getDescription() {
		return description;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}