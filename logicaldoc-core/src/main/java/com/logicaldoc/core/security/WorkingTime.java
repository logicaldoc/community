package com.logicaldoc.core.security;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

/**
 * Represents a time interval in the working time of the user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class WorkingTime implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * The day of week ranges from 1 (Sunday) to 7 (Saturday)
	 */
	private int dayOfWeek = 1;

	private int hourStart = 0;

	private int minuteStart = 0;

	private int hourEnd = 0;

	private int minuteEnd = 0;

	private String label;

	private String description;

	public WorkingTime() {

	}

	public WorkingTime(int dayOfWeek, int hourStart, int minuteStart) {
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

	public WorkingTime(WorkingTime source) {
		this.dayOfWeek = source.dayOfWeek;
		this.hourStart = source.hourStart;
		this.minuteStart = source.minuteStart;
		this.hourEnd = source.hourEnd;
		this.minuteEnd = source.minuteEnd;
		this.label = source.label;
		this.description = source.description;
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

	private Date dateStart() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.HOUR_OF_DAY, hourStart);
		cal.set(Calendar.MINUTE, minuteStart);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);
		return cal.getTime();
	}

	private Date dateEnd() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.HOUR_OF_DAY, hourEnd);
		cal.set(Calendar.MINUTE, minuteEnd);
		cal.set(Calendar.SECOND, 59);
		cal.set(Calendar.MILLISECOND, 900);
		return cal.getTime();
	}

	/**
	 * Checks if this working time matches the current time
	 * 
	 * @return true only id the current time matches the working time
	 */
	public boolean matchesCurrentTime() {
		Date now = new Date();
		Calendar cal = Calendar.getInstance();
		cal.setTime(now);
		return (getDayOfWeek() == cal.get(Calendar.DAY_OF_WEEK)) && now.after(dateStart()) && now.before(dateEnd());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + dayOfWeek;
		result = prime * result + hourEnd;
		result = prime * result + hourStart;
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
		WorkingTime other = (WorkingTime) obj;
		if (dayOfWeek != other.dayOfWeek)
			return false;
		if (hourEnd != other.hourEnd)
			return false;
		return hourStart == other.hourStart;
	}
}