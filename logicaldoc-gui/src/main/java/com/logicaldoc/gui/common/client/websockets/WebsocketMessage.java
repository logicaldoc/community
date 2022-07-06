package com.logicaldoc.gui.common.client.websockets;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;

/**
 * A websocket message that carries an event or a command to be processed by the
 * User Interface
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.1
 */
public class WebsocketMessage implements Serializable {

	private static final long serialVersionUID = 1L;

	private String sid = null;

	private String event = null;

	private Long id = null;

	private Long docId = null;

	private GUIDocument document = null;

	private Long folderId = null;

	private GUIFolder folder = null;

	private Long userId = null;

	private String username = null;

	private String comment;

	private String author;

	private Date date;

	private String command;
	
	private String payload;

	private String target;

	public WebsocketMessage(String sid, String event) {
		super();
		this.event = event;
		this.sid = sid;
	}

	public WebsocketMessage() {
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

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public Long getUserId() {
		return userId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getPayload() {
		return payload;
	}

	public void setPayload(String payload) {
		this.payload = payload;
	}

	public String getTarget() {
		return target;
	}

	public void setTarget(String target) {
		this.target = target;
	}

	public String getCommand() {
		return command;
	}

	public void setCommand(String command) {
		this.command = command;
	}
}