package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Representation of a reading request
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.6
 */
public class GUIReadingRequest implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private Date creation = new Date();

	private long docId;

	private String fileName;

	private String fileVersion;

	private long userId;

	private String userLogin = "";

	private String username = "";

	private long requestorId;

	private String requestorLogin = "";

	private String requestorName = "";

	private Date confirmed;

	private int alertConfirmation = 1;

	private String requestComment;

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getUserLogin() {
		return userLogin;
	}

	public void setUserLogin(String userLogin) {
		this.userLogin = userLogin;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public long getRequestorId() {
		return requestorId;
	}

	public void setRequestorId(long requestorId) {
		this.requestorId = requestorId;
	}

	public String getRequestorLogin() {
		return requestorLogin;
	}

	public void setRequestorLogin(String requestorLogin) {
		this.requestorLogin = requestorLogin;
	}

	public String getRequestorName() {
		return requestorName;
	}

	public void setRequestorName(String requestorName) {
		this.requestorName = requestorName;
	}

	public int getAlertConfirmation() {
		return alertConfirmation;
	}

	public void setAlertConfirmation(int alertConfirmation) {
		this.alertConfirmation = alertConfirmation;
	}

	public String getRequestComment() {
		return requestComment;
	}

	public void setRequestComment(String requestComment) {
		this.requestComment = requestComment;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (id ^ (id >>> 32));
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
		GUIReadingRequest other = (GUIReadingRequest) obj;
		if (id != other.id)
			return false;
		return true;
	}

	public Date getConfirmed() {
		return confirmed;
	}

	public void setConfirmed(Date confirmed) {
		this.confirmed = confirmed;
	}
}