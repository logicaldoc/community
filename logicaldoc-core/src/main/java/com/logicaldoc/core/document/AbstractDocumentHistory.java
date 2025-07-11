package com.logicaldoc.core.document;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Transient;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.history.History;
import com.logicaldoc.util.spring.Context;

/**
 * A superclass for those histories tightly related to documents
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2
 */
@MappedSuperclass
public abstract class AbstractDocumentHistory extends History {

	private final static Logger log = LoggerFactory.getLogger(AbstractDocumentHistory.class);

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_docid")
	private Long docId;

	@Column(name = "ld_folderid")
	private Long folderId;

	@Column(name = "ld_filesize")
	private Long fileSize = null;

	@Column(name = "ld_filename", length = 255)
	private String filename = null;

	/**
	 * Something to better qualify the event
	 */
	@Column(name = "ld_reason", length = 4000)
	private String reason = null;

	@Column(name = "ld_version", length = 10)
	private String version = null;

	@Column(name = "ld_fileversion", length = 10)
	private String fileVersion = null;

	@Column(name = "ld_pathold", length = 4000)
	private String pathOld = null;

	@Column(name = "ld_filenameold", length = 255)
	private String filenameOld = null;

	@Transient
	private AbstractDocument document;

	// Not persistent
	@Transient
	private Folder folder;

	protected AbstractDocumentHistory() {
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

	public Long getFileSize() {
		return fileSize;
	}

	public void setFileSize(Long fileSize) {
		this.fileSize = fileSize;
	}

	public String getFilename() {
		return filename;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
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

	protected void copyAttributesFrom(AbstractDocumentHistory source) {
		super.copyAttributesFrom(source);

		setDocId(source.getDocId());
		setFolderId(source.getFolderId());
		setFileSize(source.getFileSize());
		setFilename(source.getFilename());
		setReason(source.getReason());
		setVersion(source.getVersion());
		setFileVersion(source.getFileVersion());
		setPathOld(source.getPathOld());
		setFilenameOld(source.getFilenameOld());
	}

	public void setDocument(AbstractDocument document) {
		this.document = document;

		if (document != null) {
			this.setFileSize(document.getFileSize());
			this.setFilename(document.getFileName());

			if (document instanceof Version ver) {
				this.setDocId(ver.getDocId());
				this.setFolderId(ver.getFolderId());
			} else if (document instanceof Document doc) {
				this.setDocId(doc.getId());
				this.setFolderId(doc.getFolder().getId());
			}

			if (StringUtils.isEmpty(getPath()))
				try {
					setPath(Context.get(FolderDAO.class).computePathExtended(getFolderId()));
				} catch (PersistenceException e) {
					log.warn("Cannot calculate path of folder {}");
				}
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((docId == null) ? 0 : docId.hashCode());
		result = prime * result + ((folderId == null) ? 0 : folderId.hashCode());
		result = prime * result + ((version == null) ? 0 : version.hashCode());
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
		AbstractDocumentHistory other = (AbstractDocumentHistory) obj;
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
		if (version == null) {
			if (other.version != null)
				return false;
		} else if (!version.equals(other.version))
			return false;
		return true;
	}
}