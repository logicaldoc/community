package com.logicaldoc.core.document;

import javax.persistence.Table;

import com.logicaldoc.core.history.History;

/**
 * Registers an event on folder or document
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @author Marco Meschieri - LogicalDOC
 */
@Table(name = "ld_history")
public class DocumentHistory extends History {
	private static final long serialVersionUID = 1L;

	public DocumentHistory() {
		super();
	}

	public DocumentHistory(DocumentHistory source) {
		copyAttributesFrom(source);
	}
}