package com.logicaldoc.gui.common.client.beans;

/**
 * An attendee of a calendar event
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class GUIAttendee extends GUIUser {

	private static final long serialVersionUID = 1L;

	private boolean notify = true;

	private boolean required = true;

	public GUIAttendee() {
		super();
	}

	public boolean isNotify() {
		return notify;
	}

	public boolean isRequired() {
		return required;
	}

	public void setNotify(boolean notify) {
		this.notify = notify;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (notify ? 1231 : 1237);
		result = prime * result + (required ? 1231 : 1237);
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
		GUIAttendee other = (GUIAttendee) obj;
		if (notify != other.notify)
			return false;
		if (required != other.required)
			return false;
		return true;
	}
}