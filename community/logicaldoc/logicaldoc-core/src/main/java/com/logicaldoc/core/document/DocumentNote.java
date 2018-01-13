package com.logicaldoc.core.document;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A note over a document
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.2
 */
public class DocumentNote extends PersistentObject {

	private long docId;

	private long userId;

	private String username;

	private Date date;

	private String message;

	private int page = 0;

	private String snippet;

	public DocumentNote() {
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

	public String getSnippet() {
		return snippet;
	}

	public void setSnippet(String snippet) {
		this.snippet = snippet;
	}
}