package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.List;

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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((code == null) ? 0 : code.hashCode());
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
		GUIValue other = (GUIValue) obj;
		if (code == null) {
			if (other.code != null)
				return false;
		} else if (!code.equals(other.code))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return code;
	}

	public static String getValue(String code, List<GUIValue> values) {
		for (GUIValue guiValue : values) {
			if (code.equals(guiValue.getCode()))
				return guiValue.getValue();
		}
		return null;
	}
}