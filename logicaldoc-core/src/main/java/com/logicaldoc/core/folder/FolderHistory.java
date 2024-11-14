package com.logicaldoc.core.folder;

import javax.persistence.Table;

import com.logicaldoc.core.history.History;

/**
 * History entry due to an event on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
@Table(name = "ld_folder_history")
public class FolderHistory extends History {

	private static final long serialVersionUID = 1L;

	public FolderHistory() {
		super();
	}

	public FolderHistory(FolderHistory source) {
		copyAttributesFrom(source);
	}
}