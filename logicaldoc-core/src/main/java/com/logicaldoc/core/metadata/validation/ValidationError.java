package com.logicaldoc.core.metadata.validation;

import java.io.Serializable;

import org.apache.commons.lang.StringUtils;

/**
 * A simple bean to carry an error descrition
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class ValidationError implements Serializable {

	private static final long serialVersionUID = 1L;

	private String attribute;
	
	private String label;

	private String description;

	public ValidationError(String attribute, String description) {
		super();
		this.attribute = attribute;
		this.description = description;
	}
	
	public ValidationError(String attribute, String label, String description) {
		super();
		this.attribute = attribute;
		this.label = label;
		this.description = description;
	}

	public ValidationError() {
	}

	public String getAttribute() {
		return attribute;
	}

	public void setAttribute(String attribute) {
		this.attribute = attribute;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	@Override
	public String toString() {
		return (StringUtils.isNotEmpty(label) ? label : attribute) + " > " + description;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((attribute == null) ? 0 : attribute.hashCode());
		result = prime * result + ((description == null) ? 0 : description.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ValidationError other = (ValidationError) obj;
		if (attribute == null) {
			if (other.attribute != null)
				return false;
		} else if (!attribute.equals(other.attribute))
			return false;
		if (description == null) {
			if (other.description != null)
				return false;
		} else if (!description.equals(other.description))
			return false;
		return true;
	}
	
	
}