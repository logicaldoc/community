package com.logicaldoc.gui.common.client.beans;

import java.util.Date;

/**
 * Simple bean storing GUI events
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIEvent implements Comparable<GUIEvent> {

	public static final String ERROR = "error";

	public static final String WARNING = "warning";

	public static final String INFO = "info";

	private String detail = "";

	private String message = "";

	private String severity = INFO;

	private Date date = new Date();

	@Override
	public int compareTo(GUIEvent o) {
		return this.date.compareTo(o.date);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof GUIEvent))
			return false;
		GUIEvent other = (GUIEvent) obj;
		return this.date == other.date;
	}

	@Override
	public int hashCode() {
		return this.date.hashCode();
	}

	public String getDetail() {
		return detail;
	}

	public void setDetail(String detail) {
		this.detail = detail;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getSeverity() {
		return severity;
	}

	public void setSeverity(String severity) {
		this.severity = severity;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public static String getERROR() {
		return ERROR;
	}

	public static String getWARN() {
		return WARNING;
	}

	public static String getINFO() {
		return INFO;
	}
}
