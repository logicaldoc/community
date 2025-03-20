package com.logicaldoc.core.folder;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.history.AbstractDocumentHistory;

/**
 * History entry due to an event on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Entity
@Table(name = "ld_folder_history")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class FolderHistory extends AbstractDocumentHistory {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_color", length = 255)
	protected String color;

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public FolderHistory() {
		super();
	}

	public FolderHistory(FolderHistory source) {
		copyAttributesFrom(source);
		this.color = source.color;
	}
}