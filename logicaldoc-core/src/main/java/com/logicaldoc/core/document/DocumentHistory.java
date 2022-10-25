package com.logicaldoc.core.document;

import com.logicaldoc.core.History;

/**
 * Registers an event on folder or document
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @author Marco Meschieri - LogicalDOC
 */
public class DocumentHistory extends History {
	private static final long serialVersionUID = 1L;

	public DocumentHistory() {
		super();
	}

	public DocumentHistory(DocumentHistory source) {
		copyAttributesFrom(source);
	}
}