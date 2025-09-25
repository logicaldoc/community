package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

public class GUISequence implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private long value;

	// If the current sequence year is = 0, it is not a year sequence.
	private int year = 0;

	// If the current sequence month is = 0, it is not a month sequence.
	private int month = 0;

	private String name;

	private Date lastModified;

	public long getValue() {
		return value;
	}

	public void setValue(long value) {
		this.value = value;
	}

	public int getYear() {
		return year;
	}

	public void setYear(int year) {
		this.year = year;
	}

	public int getMonth() {
		return month;
	}

	public void setMonth(int month) {
		this.month = month;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}
}