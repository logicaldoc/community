package com.logicaldoc.gui.common.client.remote;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;

import de.novanic.eventservice.client.event.Event;

public class MessageEvent implements Event {
	private static final long serialVersionUID = 1L;

	private String sid = null;

	private String event = null;

	private Long docId = null;

	private GUIDocument document = null;

	private Long folderId = null;

	private GUIFolder folder = null;

	public MessageEvent(String sid, String event) {
		super();
		this.event = event;
		this.sid = sid;
	}

	public MessageEvent() {
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public String getEvent() {
		return event;
	}

	public void setEvent(String event) {
		this.event = event;
	}

	public GUIDocument getDocument() {
		return document;
	}

	public void setDocument(GUIDocument document) {
		this.document = document;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
	}

	public Long getDocId() {
		return docId;
	}

	public void setDocId(Long docId) {
		this.docId = docId;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}
}