package com.logicaldoc.core.history;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.folder.Folder;

/**
 * Superclass for history entries with more information
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
@MappedSuperclass
public abstract class ExtendedHistory extends History {

	public static final String ASPECT = "saveHistory";

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_docid")
	private Long docId;

	@Column(name = "ld_folderid")
	private Long folderId;

	/**
	 * Something to better qualify the event
	 */
	@Column(name = "ld_reason", length = 4000)
	private String reason = null;

	@Column(name = "ld_filename", length = 255)
	private String filename = null;

	@Column(name = "ld_filesize")
	private Long fileSize = null;

	// Not persistent
	@Transient
	private AbstractDocument document;

	// Not persistent
	@Transient
	private Folder folder;
	
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

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public Long getFileSize() {
		return fileSize;
	}

	public void setFileSize(Long fileSize) {
		this.fileSize = fileSize;
	}

	public void setDocument(AbstractDocument document) {
		this.document = document;
		if (document != null) {
			this.setTenantId(document.getTenantId());
			if (document instanceof Version ver)
				this.setDocId(ver.getDocId());
			else
				this.setDocId(getDocument().getId());

			this.setFilename(document.getFileName());
			this.setFileSize(document.getFileSize());

			if (document instanceof Version ver)
				this.setFolderId(ver.getFolderId());
			else if (document instanceof Document doc)
				this.setFolderId(doc.getFolder().getId());
		}
	}

	public AbstractDocument getDocument() {
		return document;
	}

	public Folder getFolder() {
		return folder;
	}

	public void setFolder(Folder folder) {
		this.folder = folder;
	}

	protected void copyAttributesFrom(ExtendedHistory source) {
		super.copyAttributesFrom(source);

		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setReason(source.getReason());
		setFilename(source.getFilename());
		setFileSize(source.getFileSize());
	}

	@Override
	public String toString() {
		return getId() + " - " + getEvent();
	}

	@Override
	public int compareTo(History other) {
		return getDate().compareTo(other.getDate());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((docId == null) ? 0 : docId.hashCode());
		result = prime * result + ((folderId == null) ? 0 : folderId.hashCode());
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
		ExtendedHistory other = (ExtendedHistory) obj;
		if (docId == null) {
			if (other.docId != null)
				return false;
		} else if (!docId.equals(other.docId))
			return false;
		if (folderId == null) {
			if (other.folderId != null)
				return false;
		} else if (!folderId.equals(other.folderId))
			return false;
		return true;
	}
}