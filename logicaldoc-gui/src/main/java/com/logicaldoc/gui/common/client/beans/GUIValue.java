package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * General purpose value bean
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIValue implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private String code;
	
	private String value;

	public GUIValue() {

	}

	public GUIValue(String code, String value) {
		super();
		this.code = code;
		this.value = value;
	}
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}