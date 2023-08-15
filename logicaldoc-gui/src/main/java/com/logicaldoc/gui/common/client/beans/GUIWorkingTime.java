package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * A working time slot to be used in the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class GUIWorkingTime implements Serializable, Comparable<GUIWorkingTime> {

	private static final long serialVersionUID = 1L;

	private Date start;

	private Date end;

	private String label;

	private String description;

	public GUIWorkingTime() {

	}

	public GUIWorkingTime(String label, Date start, Date end) {
		super();
		this.start = start;
		this.end = end;
		this.label = label;
	}

	public Date getStart() {
		return start;
	}

	public void setStart(Date start) {
		this.start = start;
	}

	public Date getEnd() {
		return end;
	}

	public void setEnd(Date end) {
		this.end = end;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public int compareTo(GUIWorkingTime o) {
		return this.start.compareTo(o.start);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof GUIWorkingTime))
			return false;
		GUIWorkingTime other = (GUIWorkingTime) obj;
		return this.start.equals(other.start);
	}

	@Override
	public int hashCode() {
		return this.start.hashCode();
	}
}