package com.logicaldoc.core.document;

/**
 * Basic concrete implementation of <code>AbstractDocument</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class Document extends AbstractDocument {
	private static final long serialVersionUID = 1L;
	
	// Useful but not persisted
	private Long templateId;

	public Document() {
	}
	
	public Document(Document source) {
		copyAttributes(source);
		setId(source.getId());
		setOcrd(source.getOcrd());
		setOcrTemplateId(source.getOcrTemplateId());
		setBarcoded(source.getBarcoded());
		setBarcodeTemplateId(source.getBarcodeTemplateId());
		if (getIndexed() != INDEX_INDEXED)
			setIndexed(source.getIndexed());
		setCustomId(null);
	}

	public Long getTemplateId() {
		if (templateId != null)
			return templateId;
		else if (getTemplate() != null)
			return getTemplate().getId();
		else
			return null;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}
}