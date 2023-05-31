package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Represents a reminder of a calendar event
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class GUIReminder implements Comparable<GUIReminder>, Serializable {

	private static final long serialVersionUID = 1L;

	public final static String TIME_UNIT_MINUTE = "minute";

	public final static String TIME_UNIT_HOUR = "hour";

	public final static String TIME_UNIT_DAY = "day";

	/**
	 * Time before the event start date when the recipients need to be notified.
	 * The unit is expressed in remindUnit.
	 */
	private int value = 0;

	private String unit = TIME_UNIT_MINUTE;

	/**
	 * The exact date when the event must be notified
	 */
	private Date date = null;

	private int reminded = 0;

	public GUIReminder() {

	}

	public GUIReminder(int value, String unit) {
		super();
		this.value = value;
		this.unit = unit;
	}

	public String getUnit() {
		return unit;
	}

	public void setUnit(String unit) {
		this.unit = unit;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public int getReminded() {
		return reminded;
	}

	public void setReminded(int reminded) {
		this.reminded = reminded;
	}

	public int getValue() {
		return value;
	}

	public void setValue(int value) {
		this.value = value;
	}

	@Override
	public int compareTo(GUIReminder other) {
		if (this.date == null && other != null)
			return -1;
		else if (this.date != null && other == null)
			return 1;
		else if (this.date == null && other == null)
			return 0;
		else
			return this.date.compareTo(other.date);
	}
}