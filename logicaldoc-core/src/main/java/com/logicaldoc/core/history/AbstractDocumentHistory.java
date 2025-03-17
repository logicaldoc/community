package com.logicaldoc.core.history;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

import com.logicaldoc.core.document.AbstractDocument;

/**
 * A superclass for those histories tightly related to documents
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2
 */
@MappedSuperclass
public abstract class AbstractDocumentHistory extends History {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_color", length = 255)
	protected String color;

	@Column(name = "ld_version", length = 10)
	protected String version = null;

	@Column(name = "ld_fileversion", length = 10)
	protected String fileVersion = null;

	@Column(name = "ld_pathold", length = 4000)
	protected String pathOld = null;

	@Column(name = "ld_filenameold", length = 255)
	protected String filenameOld = null;

	public AbstractDocumentHistory() {
		super();
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
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

	@Override
	public void setDocument(AbstractDocument document) {
		super.setDocument(document);
		if (document != null) {
			this.setVersion(document.getVersion());
			this.setFileVersion(document.getFileVersion());
			this.setColor(document.getColor());
		}
	}

	protected void copyAttributesFrom(AbstractDocumentHistory source) {
		super.copyAttributesFrom(source);
		setColor(source.getColor());
		setVersion(source.getVersion());
		setFileVersion(source.getFileVersion());
		setPathOld(source.getPathOld());
		setFilenameOld(source.getFilenameOld());
	}
}