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

	private Date startDate = new Date();

	private Date expirationDate = null;

	private List<GUIUser> participants = new ArrayList<>();

	private List<GUIGroup> participantsGroups = new ArrayList<>();

	public List<GUIGroup> getParticipantsGroups() {
		return participantsGroups;
	}

	public void setParticipantsGroups(List<GUIGroup> participantsGroups) {
		this.participantsGroups = participantsGroups;
	}

	private List<GUIDocument> documents = new ArrayList<>();

	private List<GUIReminder> reminders = new ArrayList<>();

	private int frequency = 0;

	private long creatorId;

	private String creator;

	private int status = 0;

	private Date completionDate;

	private Date deadline;

	private String automation;

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

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Date expirationDate) {
		this.expirationDate = expirationDate;
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

	public GUIUser getParticipant(long id) {
		for (GUIUser guiUser : participants) {
			if (guiUser.getId() == id)
				return guiUser;
		}
		return null;
	}

	public GUIGroup getParticipantGroup(long id) {
		for (GUIGroup guiGroup : participantsGroups) {
			if (guiGroup.getId() == id)
				return guiGroup;
		}
		return null;
	}

	public void addParticipant(GUIUser newPart) {
		if (getParticipant(newPart.getId()) != null)
			return;
		participants.add(newPart);
	}

	public void addParticipant(GUIGroup newPart) {
		if (getParticipantGroup(newPart.getId()) != null)
			return;
		participantsGroups.add(newPart);
	}

	public void removeParticipant(long id) {
		List<GUIUser> newParts = new ArrayList<>();
		for (GUIUser guiUser : participants) {
			if (id != guiUser.getId())
				newParts.add(guiUser);
		}
		participants = newParts;
	}

	public void removeParticipantGroup(long id) {
		List<GUIGroup> newParts = new ArrayList<>();
		for (GUIGroup guiGroup : participantsGroups) {
			if (id != guiGroup.getId())
				newParts.add(guiGroup);
		}
		participantsGroups = newParts;

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

	public long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(long creatorId) {
		this.creatorId = creatorId;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public List<GUIUser> getParticipants() {
		return participants;
	}

	public void setParticipants(List<GUIUser> participants) {
		this.participants = participants;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public Date getCompletionDate() {
		return completionDate;
	}

	public void setCompletionDate(Date completionDate) {
		this.completionDate = completionDate;
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
}