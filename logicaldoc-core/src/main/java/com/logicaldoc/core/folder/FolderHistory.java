package com.logicaldoc.core.folder;

import com.logicaldoc.core.History;

/**
 * History entry due to an event on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class FolderHistory extends History {

	private static final long serialVersionUID = 1L;

	public FolderHistory() {
		super();
	}

	public FolderHistory(FolderHistory source) {
		copyAttributesFrom(source);
	}
}