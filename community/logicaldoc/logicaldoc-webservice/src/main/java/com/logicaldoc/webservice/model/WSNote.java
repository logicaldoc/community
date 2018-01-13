package com.logicaldoc.webservice.model;

import java.util.Date;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Note.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@XmlType(name = "WSNote")
public class WSNote {

	@WSDoc(description = "unique identifier")
	private long id;

	@WSDoc(description = "the referenced document")
	private long docId;

	@WSDoc(description = "id of the user that created the note")
	private long userId;

	@WSDoc(description = "name of the user that created the note")
	private String username;

	private Date date;

	private String message;

	private int page = 0;

	public DocumentNote toDocumentNote() {
		DocumentNote note = new DocumentNote();
		note.setId(getId());
		note.setDate(getDate());
		note.setDocId(getDocId());
		note.setMessage(getMessage());
		note.setUserId(getUserId());
		note.setUsername(getUsername());
		note.setPage(getPage());
		return note;
	}

	public static WSNote fromDocumentNote(DocumentNote note) {
		WSNote wsNote = new WSNote();
		wsNote.setDate(note.getDate());
		wsNote.setDocId(note.getDocId());
		wsNote.setId(note.getId());
		wsNote.setMessage(note.getMessage());
		wsNote.setUserId(note.getUserId());
		wsNote.setUsername(note.getUsername());
		wsNote.setPage(note.getPage());
		return wsNote;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
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

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}
}