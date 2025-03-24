package com.logicaldoc.core.history;

import java.util.Objects;

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
	
	@Column(name = "ld_filename", length = 255)
	private String filename = null;

	@Column(name = "ld_filesize")
	private Long fileSize = null;
	
	@Column(name = "ld_folderid")
	private Long folderId;

	/**
	 * Something to better qualify the event
	 */
	@Column(name = "ld_reason", length = 4000)
	private String reason = null;

	// Not persistent
	@Transient
	private AbstractDocument document;

	// Not persistent
	@Transient
	private Folder folder;

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
		result = prime * result + Objects.hash(folderId);
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
		return Objects.equals(folderId, other.folderId);
	}
}