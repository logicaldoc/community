package com.logicaldoc.core.document;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

/**
 * Registers an event on folder or document
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @author Marco Meschieri - LogicalDOC
 */
@Entity
@Table(name = "ld_history")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class DocumentHistory extends AbstractDocumentHistory {
	private static final long serialVersionUID = 1L;

	@Column(name = "ld_color", length = 255)
	protected String color;

	@Column(name = "ld_new")
	private int isNew = 1;
	
	@Transient
	private String file = null;

	public DocumentHistory() {
		super();
	}
	
	public DocumentHistory(DocumentHistory source) {
		copyAttributesFrom(source);
		this.color = source.color;
		setFile(source.getFile());
		setIsNew(source.getIsNew());
	}
	
	public String getFile() {
		return file;
	}

	public void setFile(String file) {
		this.file = file;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public int getIsNew() {
		return isNew;
	}

	public void setIsNew(int isNew) {
		this.isNew = isNew;
	}

	@Override
	public void setDocument(AbstractDocument document) {
		super.setDocument(document);
		if (document != null)
			this.color = document.getColor();
	}
}