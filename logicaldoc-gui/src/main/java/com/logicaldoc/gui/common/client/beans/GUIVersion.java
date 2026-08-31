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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (docId ^ (docId >>> 32));
		result = prime * result + ((versionDate == null) ? 0 : versionDate.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		GUIVersion other = (GUIVersion) obj;
		if (docId != other.docId)
			return false;
		if (versionDate == null) {
			if (other.versionDate != null)
				return false;
		} else if (!versionDate.equals(other.versionDate))
			return false;
		return true;
	}
}