package com.logicaldoc.core.folder;

import com.logicaldoc.core.document.AbstractDocumentHistory;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

/**
 * History entry due to an event on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Entity
@Table(name = "ld_folder_history")
@Cacheable
public class FolderHistory extends AbstractDocumentHistory {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_color", length = 255)
	protected String color;

	public void setEvent(FolderEvent event) {
		this.event = (event != null) ? event.toString() : null;
	}
	
	@Override
	public void setEvent(String event) {
		setEvent(FolderEvent.fromKey(event));
	}

	public FolderEvent getEventEnum() {
		if (event == null)
			return null;
		return FolderEvent.fromKey(event);
	}

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