package com.logicaldoc.core.history;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.folder.Folder;

/**
 * A superclass for those histories tightly related to documents
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2
 */
@MappedSuperclass
public abstract class AbstractDocumentHistory extends History {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_version", length = 10)
	protected String version = null;

	@Column(name = "ld_fileversion", length = 10)
	protected String fileVersion = null;

	@Column(name = "ld_pathold", length = 4000)
	protected String pathOld = null;

	@Column(name = "ld_filenameold", length = 255)
	protected String filenameOld = null;
	
	@Column(name = "ld_docid")
	private Long docId;

	@Column(name = "ld_folderid")
	private Long folderId;

	@Column(name = "ld_filename", length = 255)
	private String filename = null;

	@Column(name = "ld_filesize")
	private Long fileSize = null;
	
	@Column(name = "ld_new")
	private int isNew = 1;

	/**
	 * Something to better qualify the event
	 */
	@Column(name = "ld_reason")
	private String reason = null;
	
	// Not persistent
	@Transient
	private AbstractDocument document;

	// Not persistent
	@Transient
	private Folder folder;


	public AbstractDocumentHistory() {
		super();
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

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getPathOld() {
		return pathOld;
	}

	public void setPathOld(String pathOld) {
		this.pathOld = pathOld;
	}

	public String getFilenameOld() {
		return filenameOld;
	}

	public void setFilenameOld(String filenameOld) {
		this.filenameOld = filenameOld;
	}
	
	public int getIsNew() {
		return isNew;
	}

	public void setIsNew(int isNew) {
		this.isNew = isNew;
	}

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
	}

	public Folder getFolder() {
		return folder;
	}

	public void setFolder(Folder folder) {
		this.folder = folder;
	}

	public AbstractDocument getDocument() {
		return document;
	}

	public void setDocument(AbstractDocument document) {
		this.document = document;
		if (document != null) {
			this.setTenantId(document.getTenantId());
			if (document instanceof Version ver)
				this.setDocId(ver.getDocId());
			else
				this.setDocId(document.getId());

			this.setFilename(document.getFileName());
			this.setFileSize(document.getFileSize());

			if (document instanceof Version ver)
				this.setFolderId(ver.getFolderId());
			else if (document instanceof Document doc)
				this.setFolderId(doc.getFolder().getId());
		}
	}


	protected void copyAttributesFrom(AbstractDocumentHistory source) {
		super.copyAttributesFrom(source);
		setVersion(source.getVersion());
		setFileVersion(source.getFileVersion());
		setPathOld(source.getPathOld());
		setFilenameOld(source.getFilenameOld());
		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setReason(source.getReason());
		setIsNew(source.getIsNew());
		setFilename(source.getFilename());
		setFileSize(source.getFileSize());
	}
}