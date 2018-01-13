package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.Constants;

/**
 * This class represents an event in a calendar. An event is always associated
 * to a selection of documents and users.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.7
 */
public class GUICalendarEvent implements Serializable {

	private static final long serialVersionUID = 1L;

	public final static int STATUS_NONE = 0;

	public final static int STATUS_WORKING = 1;

	public final static int STATUS_COMPLETED = 2;

	public final static int STATUS_CANCELED = 3;

	private long id = 0;

	private Long parentId = null;

	private String title = "";

	private String type = "";

	private String subType = "";

	private String description = "";

	private Date startDate = new Date();

	private Date expirationDate = null;

	private GUIUser[] participants = new GUIUser[0];

	private GUIDocument[] documents = new GUIDocument[0];

	private int frequency = 0;

	private int remindTime = 1;

	private String remindUnit = Constants.TIME_HOUR;

	private long creatorId;

	private String creator;

	private int status = 0;

	private Date completionDate;

	private Date deadline;

	public GUICalendarEvent() {
	}

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

	public GUIDocument[] getDocuments() {
		return documents;
	}

	public void setDocuments(GUIDocument[] documents) {
		this.documents = documents;
	}

	public int getFrequency() {
		return frequency;
	}

	public void setFrequency(int frequency) {
		this.frequency = frequency;
	}

	public GUIUser getParticipant(long id) {
		for (int i = 0; i < participants.length; i++) {
			if (participants[i].getId() == id)
				return participants[i];
		}
		return null;
	}

	public void addParticipant(GUIUser newPart) {
		if (getParticipant(newPart.getId()) != null)
			return;
		GUIUser[] newParts = new GUIUser[participants.length + 1];
		for (int i = 0; i < participants.length; i++) {
			newParts[i] = participants[i];
		}
		newParts[participants.length] = newPart;
		participants = newParts;
	}

	public void removeParticipant(long id) {
		GUIUser[] newParts = new GUIUser[participants.length - 1];
		int j = 0;
		for (int i = 0; i < participants.length; i++) {
			if (id == participants[i].getId())
				continue;
			newParts[j++] = participants[i];
		}
		participants = newParts;
	}

	public GUIDocument getDocument(long id) {
		for (int i = 0; i < documents.length; i++) {
			if (documents[i].getId() == id)
				return documents[i];
		}
		return null;
	}

	public void addDocument(GUIDocument newDoc) {
		if (getDocument(newDoc.getId()) != null)
			return;
		GUIDocument[] newDocs = new GUIDocument[documents.length + 1];
		for (int i = 0; i < documents.length; i++) {
			newDocs[i] = documents[i];
		}
		newDocs[documents.length] = newDoc;
		documents = newDocs;
	}

	public void removeDocument(long docId) {
		GUIDocument[] newDocs = new GUIDocument[documents.length - 1];
		int j = 0;
		for (int i = 0; i < documents.length; i++) {
			if (docId == documents[i].getId())
				continue;
			newDocs[j++] = documents[i];
		}
		documents = newDocs;
	}

	public int getRemindTime() {
		return remindTime;
	}

	public void setRemindTime(int remindTime) {
		this.remindTime = remindTime;
	}

	public String getRemindUnit() {
		return remindUnit;
	}

	public void setRemindUnit(String remindUnit) {
		this.remindUnit = remindUnit;
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

	public GUIUser[] getParticipants() {
		return participants;
	}

	public void setParticipants(GUIUser[] participants) {
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
}