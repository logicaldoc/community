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

	/**
	 * Clones the document but does not replicate the CustomID
	 */
	@Override
	public Document clone() {
		Document cloned = new Document();
		cloned.copyAttributes((Document) this);
		cloned.setId(getId());
		cloned.setOcrd(getOcrd());
		cloned.setOcrTemplateId(getOcrTemplateId());
		cloned.setBarcoded(getBarcoded());
		cloned.setBarcodeTemplateId(getBarcodeTemplateId());
		if (getIndexed() != INDEX_INDEXED)
			cloned.setIndexed(getIndexed());
		cloned.setCustomId(null);
		return cloned;
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