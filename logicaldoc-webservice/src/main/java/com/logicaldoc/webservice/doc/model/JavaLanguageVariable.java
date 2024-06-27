package com.logicaldoc.webservice.doc.model;

import static org.apache.commons.lang.builder.ToStringStyle.SHORT_PREFIX_STYLE;

import org.apache.commons.lang.builder.ToStringBuilder;

import com.logicaldoc.webservice.doc.DocException;

/**
 * Imagine a Java Language Variable = a Field in a Java Class, or a Parameter in
 * a Java Method
 * 
 */
public class JavaLanguageVariable {

	/**
	 * as "order" in @XmlType(name = "order"). If the name is not specified in
	 * 
	 * @XmlType, use the field name
	 */
	private String variableName;

	/**
	 * the java type of a variable, such as "Order.class"
	 */
	private Class<?> type;

	private boolean required;

	private boolean multiOccurs;

	private String description = "";

	public Class<?> getType() {
		return type;
	}

	public void setType(Class<?> type) {
		if (type == null)
			throw new DocException("null type");
		this.type = type;
	}

	public String getVariableName() {
		return variableName;
	}

	public void setVariableName(String name) {
		this.variableName = name;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, SHORT_PREFIX_STYLE).append(type).append(variableName).toString();
	}

	public boolean isMultiOccurs() {
		return multiOccurs;
	}

	public void setMultiOccurs(boolean multiOccurs) {
		this.multiOccurs = multiOccurs;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

}
