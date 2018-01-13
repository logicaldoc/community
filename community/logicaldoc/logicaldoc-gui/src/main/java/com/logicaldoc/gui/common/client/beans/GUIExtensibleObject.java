package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Main class for extensible objects
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	private String template;

	private Long templateId;

	private GUIAttribute[] attributes = new GUIAttribute[0];

	public String getTemplate() {
		return template;
	}

	public void setTemplate(String template) {
		this.template = template;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public GUIAttribute[] getAttributes() {
		return attributes;
	}

	public void setAttributes(GUIAttribute[] attributes) {
		this.attributes = attributes;
	}

	public Object getValue(String attributeName) {
		for (GUIAttribute att : attributes) {
			if (att.getName().equals(attributeName) && att.getValue() != null)
				return att.getValue();
		}
		return null;
	}

	public GUIAttribute getAttribute(String attributeName) {
		for (GUIAttribute att : attributes) {
			if (att.getName().equals(attributeName))
				return att;
		}
		return null;
	}

	public GUIAttribute setValue(String name, Object value) {
		GUIAttribute[] tmp = new GUIAttribute[attributes.length + 1];
		int i = 0;
		for (GUIAttribute a : attributes) {
			tmp[i++] = a;
		}

		GUIAttribute ext = new GUIAttribute();
		ext.setName(name);
		ext.setValue(value);
		tmp[i] = ext;
		attributes = tmp;
		return ext;
	}
}