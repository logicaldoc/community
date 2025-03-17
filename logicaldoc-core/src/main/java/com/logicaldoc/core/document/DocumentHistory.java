package com.logicaldoc.core.document;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

import com.logicaldoc.core.history.AbstractDocumentHistory;

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

	public DocumentHistory() {
		super();
	}

	public DocumentHistory(DocumentHistory source) {
		copyAttributesFrom(source);
	}

	@Override
	public void setDocument(AbstractDocument document) {
		super.setDocument(document);
		if (document != null)
			this.color = document.getColor();
	}
}