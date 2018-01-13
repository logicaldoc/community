package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

import com.logicaldoc.gui.common.client.i18n.I18N;

public class GUIParameter implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	private String value;

	private String label;

	public GUIParameter() {
	}

	public GUIParameter(String name, String value) {
		super();
		this.name = name;
		this.value = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String toString() {
		if (label != null)
			return I18N.message(label);
		else
			return I18N.message(name);
	}
}
