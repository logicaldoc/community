package com.logicaldoc.core.document;

import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.security.AccessControlEntry;

/**
 * Basic concrete implementation of <code>AbstractDocument</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class Document extends AbstractDocument {

	private static final long serialVersionUID = 1L;

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
			for (AccessControlEntry ace : source.getAccessControlList())
				getAccessControlList().add(new AccessControlEntry(ace));
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}
}