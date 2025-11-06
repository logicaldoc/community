package com.logicaldoc.webservice.model;

import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a time interval in the working time of the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
@XmlType(name = "WSWorkingTime")
public class WSWorkingTime {

	@WSDoc(description = "the day of week ranges from 1 (Sunday) to 7 (Saturday)", required = true)
	private int dayOfWeek = 1;

	@WSDoc(description = "the start hour in the day (0-23)", required = true)
	private int hourStart = 0;

	@WSDoc(description = "the start minute in the day (0-59)", required = true)
	private int minuteStart = 0;

	@WSDoc(description = "the end hour in the day (0-23)", required = true)
	private int hourEnd = 0;

	@WSDoc(description = "the end minute in the day (0-59)", required = true)
	private int minuteEnd = 0;

	@WSDoc(description = "an optional label to give to this working time", required = false)
	private String label;

	@WSDoc(description = "an optional description to give to this working time", required = false)
	private String description;

	public WSWorkingTime() {

	}

	public WSWorkingTime(int dayOfWeek, int hourStart, int minuteStart) {
		super();
		this.dayOfWeek = dayOfWeek;
		this.hourStart = hourStart;
		if (hourStart < 23)
			this.hourEnd = hourStart + 1;
		else
			this.hourEnd = hourStart;
		this.minuteStart = minuteStart;
		this.minuteEnd = minuteStart;
	}

	public int getDayOfWeek() {
		return dayOfWeek;
	}

	public void setDayOfWeek(int dayOfWeek) {
		this.dayOfWeek = dayOfWeek;
	}

	public int getHourStart() {
		return hourStart;
	}

	public void setHourStart(int hourStart) {
		this.hourStart = hourStart;
	}

	public int getMinuteStart() {
		return minuteStart;
	}

	public void setMinuteStart(int minuteStart) {
		this.minuteStart = minuteStart;
	}

	public int getHourEnd() {
		return hourEnd;
	}

	public void setHourEnd(int hourEnd) {
		this.hourEnd = hourEnd;
	}

	public int getMinuteEnd() {
		return minuteEnd;
	}

	public void setMinuteEnd(int minuteEnd) {
		this.minuteEnd = minuteEnd;
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
}