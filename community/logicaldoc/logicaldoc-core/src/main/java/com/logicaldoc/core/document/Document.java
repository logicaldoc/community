package com.logicaldoc.core.document;

import java.util.HashMap;
import java.util.HashSet;

import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.metadata.Attribute;

/**
 * Basic concrete implementation of <code>AbstractDocument</code>
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 1.0
 */
public class Document extends AbstractDocument {
	// Useful but not persisted
	public Long templateId;

	public Document() {
	}

	/**
	 * Copies in the current instance the attributes of the passed values
	 * object, but NOT the ID.
	 */
	public void copyAttributes(Document docVO) {
		setTenantId(docVO.getTenantId());
		setCustomId(docVO.getCustomId());
		setImmutable(docVO.getImmutable());
		setVersion(docVO.getVersion());
		setFileVersion(docVO.getFileVersion());
		setDate(docVO.getDate());
		setPublisher(docVO.getPublisher());
		setPublisherId(docVO.getPublisherId());
		setCreator(docVO.getCreator());
		setCreatorId(docVO.getCreatorId());
		setStatus(docVO.getStatus());
		setType(docVO.getType());
		setLockUserId(docVO.getLockUserId());
		setLanguage(docVO.getLanguage());
		setFileName(docVO.getFileName());
		setFileSize(docVO.getFileSize());
		setIndexed(docVO.getIndexed());
		setBarcoded(docVO.getBarcoded());
		setDigest(docVO.getDigest());
		setDocRef(docVO.getDocRef());
		setFolder(docVO.getFolder());
		setTemplate(docVO.getTemplate());
		setPages(docVO.getPages());

		setAttributes(new HashMap<String, Attribute>());
		try {
			for (String name : docVO.getAttributes().keySet()) {
				getAttributes().put(name, docVO.getAttributes().get(name));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}

		try {
			setTags(new HashSet<Tag>());
			for (Tag tag : docVO.getTags()) {
				getTags().add(tag);
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		Document cloned = new Document();
		cloned.copyAttributes((Document) this);
		cloned.setId(getId());
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