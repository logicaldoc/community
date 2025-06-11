package com.logicaldoc.webservice.model;

import java.io.Serializable;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

/**
 * An option for a preset attribute
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
@XmlRootElement(name = "attribute")
@XmlType(name = "WSAttributeOption")
public class WSAttributeOption implements Serializable {

	private static final long serialVersionUID = 1L;

	@WSDoc(required = true, description = "the value")
	private String value;

	@WSDoc(required = false, description = "the category")
	private String category;

	public WSAttributeOption() {
	}
	
	public WSAttributeOption(String value, String category) {
		super();
		this.value = value;
		this.category = category;
	}

	public WSAttributeOption(String value) {
		super();
		this.value = value;
	}		

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		WSAttributeOption other = (WSAttributeOption) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
}