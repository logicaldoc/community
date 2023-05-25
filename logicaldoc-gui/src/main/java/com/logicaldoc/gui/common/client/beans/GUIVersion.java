package com.logicaldoc.gui.common.client.beans;

import java.util.Date;

/**
 * Represents a document version
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIVersion extends GUIDocument {

	private static final long serialVersionUID = 1L;

	private long docId;

	private String comment;

	private String username;

	private Date versionDate = new Date();

	private int signed = 0;

	// Used to show error message on the archived version
	private String errorText;

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	@Override
	public String getComment() {
		return comment;
	}

	@Override
	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Date getVersionDate() {
		return versionDate;
	}

	public void setVersionDate(Date versionDate) {
		this.versionDate = versionDate;
	}

	@Override
	public int getSigned() {
		return signed;
	}

	@Override
	public void setSigned(int signed) {
		this.signed = signed;
	}

	public String getErrorText() {
		return errorText;
	}

	public void setErrorText(String errorText) {
		this.errorText = errorText;
	}
}