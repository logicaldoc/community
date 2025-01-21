package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * This class represents an event in a calendar. An event is always associated
 * to a selection of documents and users.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class GUICalendarEvent implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int STATUS_NONE = 0;

	public static final int STATUS_WORKING = 1;

	public static final int STATUS_COMPLETED = 2;

	public static final int STATUS_CANCELED = 3;

	private long id = 0;

	private Long parentId = null;

	private String externalId = "";

	private String externalUrl = "";

	private String title = "";

	private String type = "";

	private String subType = "";

	private String description = "";

	private String location = "";

	private Date start = new Date();

	private Date end = null;

	private List<GUIAttendee> attendees = new ArrayList<>();

	private List<GUIGroup> attendeesGroups = new ArrayList<>();

	private List<GUIDocument> documents = new ArrayList<>();

	private List<GUIReminder> reminders = new ArrayList<>();

	private int frequency = 0;

	private long organizerId;

	private String organizer;

	private int status = 0;

	private Date deadline;

	private String automation;

	private boolean iCalendar = true;

	@Override
	public String toString() {
		return getId() + "-" + getTitle();
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public Long getParentId() {
		return parentId;
	}

	public void setParentId(Long parentId) {
		this.parentId = parentId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getStart() {
		return start;
	}

	public void setStart(Date startDate) {
		this.start = startDate;
	}

	public Date getEnd() {
		return end;
	}

	public void setEnd(Date expirationDate) {
		this.end = expirationDate;
	}

	public List<GUIDocument> getDocuments() {
		return documents;
	}

	public void setDocuments(List<GUIDocument> documents) {
		this.documents = documents;
	}

	public int getFrequency() {
		return frequency;
	}

	public void setFrequency(int frequency) {
		this.frequency = frequency;
	}

	public GUIUser getAttendee(long id) {
		for (GUIUser guiUser : attendees) {
			if (guiUser.getId() == id)
				return guiUser;
		}
		return null;
	}

	public GUIGroup getAttendeeGroup(long id) {
		for (GUIGroup guiGroup : attendeesGroups) {
			if (guiGroup.getId() == id)
				return guiGroup;
		}
		return null;
	}

	public void addAttendee(GUIAttendee newPart) {
		if (getAttendee(newPart.getId()) != null)
			return;
		attendees.add(newPart);
	}

	public void addAttendee(GUIGroup newPart) {
		if (getAttendeeGroup(newPart.getId()) != null)
			return;
		attendeesGroups.add(newPart);
	}

	public void removeAttendee(long id) {
		List<GUIAttendee> newParts = new ArrayList<>();
		for (GUIAttendee guiUser : attendees) {
			if (id != guiUser.getId())
				newParts.add(guiUser);
		}
		attendees = newParts;
	}

	public void removeAttendeeGroup(long id) {
		List<GUIGroup> newParts = new ArrayList<>();
		for (GUIGroup guiGroup : attendeesGroups) {
			if (id != guiGroup.getId())
				newParts.add(guiGroup);
		}
		attendeesGroups = newParts;

	}

	public GUIDocument getDocument(long id) {
		for (GUIDocument guiDocument : documents) {
			if (guiDocument.getId() == id)
				return guiDocument;
		}
		return null;
	}

	public void addDocument(GUIDocument newDoc) {
		if (getDocument(newDoc.getId()) != null)
			return;
		documents.add(newDoc);
	}

	public void removeDocument(long docId) {
		List<GUIDocument> newDocs = new ArrayList<>();
		for (GUIDocument guiDocument : documents) {
			if (docId != guiDocument.getId())
				newDocs.add(guiDocument);
		}
		documents = newDocs;
	}

	public void addReminder(GUIReminder reminder) {
		reminders.add(reminder);
	}

	public long getOrganizerId() {
		return organizerId;
	}

	public void setOrganizerId(long creatorId) {
		this.organizerId = creatorId;
	}

	public String getOrganizer() {
		return organizer;
	}

	public void setOrganizer(String creator) {
		this.organizer = creator;
	}

	public List<GUIAttendee> getAttendees() {
		return attendees;
	}

	public void setAttendees(List<GUIAttendee> attendees) {
		this.attendees = attendees;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public Date getDeadline() {
		return deadline;
	}

	public void setDeadline(Date deadline) {
		this.deadline = deadline;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getSubType() {
		return subType;
	}

	public void setSubType(String subType) {
		this.subType = subType;
	}

	public List<GUIReminder> getReminders() {
		return reminders;
	}

	public void setReminders(List<GUIReminder> reminders) {
		this.reminders = reminders;
	}

	public String getAutomation() {
		return automation;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public String getExternalId() {
		return externalId;
	}

	public void setExternalId(String externalId) {
		this.externalId = externalId;
	}

	public String getExternalUrl() {
		return externalUrl;
	}

	public void setExternalUrl(String externalUrl) {
		this.externalUrl = externalUrl;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public List<GUIGroup> getAttendeesGroups() {
		return attendeesGroups;
	}

	public void setAttendeesGroups(List<GUIGroup> attendeesGroups) {
		this.attendeesGroups = attendeesGroups;
	}

	public boolean isiCalendar() {
		return iCalendar;
	}

	public void setiCalendar(boolean iCalendar) {
		this.iCalendar = iCalendar;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ (id >>> 32));
		result = prime * result + ((start == null) ? 0 : start.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
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
		GUICalendarEvent other = (GUICalendarEvent) obj;
		if (id != other.id)
			return false;
		if (start == null) {
			if (other.start != null)
				return false;
		} else if (!start.equals(other.start))
			return false;
		if (title == null) {
			if (other.title != null)
				return false;
		} else if (!title.equals(other.title))
			return false;
		return true;
	}
}