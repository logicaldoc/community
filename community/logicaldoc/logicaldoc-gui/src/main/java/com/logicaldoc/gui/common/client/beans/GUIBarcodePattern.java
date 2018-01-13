package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

public class GUIBarcodePattern implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long templateId;

	private String pattern;

	private int position = 0;

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public String getPattern() {
		return pattern;
	}

	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
}