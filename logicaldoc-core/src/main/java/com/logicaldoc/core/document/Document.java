package com.logicaldoc.core.document;

import java.util.HashSet;
import java.util.Set;

import org.hibernate.LazyInitializationException;

/**
 * Basic concrete implementation of <code>AbstractDocument</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class Document extends AbstractDocument {
	private static final long serialVersionUID = 1L;

	private Set<DocumentGroup> documentGroups = new HashSet<>();

	public Document() {
	}

	public Document(Document source) {
		copyAttributes(source);
		setId(source.getId());
		setOcrd(source.getOcrd());
		setOcrTemplateId(source.getOcrTemplateId());
		setBarcoded(source.getBarcoded());
		setBarcodeTemplateId(source.getBarcodeTemplateId());
		setTemplate(source.getTemplate());
		setTemplateId(source.getTemplateId());
		setTemplateName(source.getTemplateName());

		if (source.getIndexed() != INDEX_INDEXED)
			setIndexed(source.getIndexed());
		setCustomId(null);

		try {
			setDocumentGroups(new HashSet<>());
			for (DocumentGroup dg : source.getDocumentGroups()) {
				getDocumentGroups().add(new DocumentGroup(dg));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	public Set<DocumentGroup> getDocumentGroups() {
		return documentGroups;
	}

	public void setDocumentGroups(Set<DocumentGroup> documentGroups) {
		this.documentGroups = documentGroups;
	}
}