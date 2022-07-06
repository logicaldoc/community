package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

public class GUICriterion implements Serializable {

	private static final long serialVersionUID = 1L;

	private String field;

	private String operator;

	private String stringValue;

	private Date dateValue;

	private Long longValue;

	private Double doubleValue;

	public GUICriterion() {
	}

	public String getField() {
		return field;
	}

	public void setField(String field) {
		this.field = field;
	}

	public String getOperator() {
		return operator;
	}

	public void setOperator(String operator) {
		this.operator = operator;
	}

	public String getStringValue() {
		return stringValue;
	}

	public void setStringValue(String value) {
		this.stringValue = value;
	}

	public Date getDateValue() {
		return dateValue;
	}

	public void setDateValue(Date dateValue) {
		this.dateValue = dateValue;
	}

	public Long getLongValue() {
		return longValue;
	}

	public void setLongValue(Long longValue) {
		this.longValue = longValue;
	}

	public Double getDoubleValue() {
		return doubleValue;
	}

	public void setDoubleValue(Double doubleValue) {
		this.doubleValue = doubleValue;
	}
}