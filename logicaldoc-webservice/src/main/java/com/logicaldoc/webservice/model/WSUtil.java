package com.logicaldoc.webservice.model;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.time.DateUtil;

public class WSUtil {
	protected static Logger log = LoggerFactory.getLogger(WSUtil.class);

	private WSUtil() {
	}

	public static WSDocument toWSDocument(AbstractDocument document) {
		WSDocument wsDoc = new WSDocument();

		try {
			wsDoc.setId(document.getId());
			wsDoc.setCustomId(document.getCustomId());
			wsDoc.setLanguage(document.getLanguage());
			wsDoc.setComment(document.getComment());
			wsDoc.setWorkflowStatus(document.getWorkflowStatus());
			wsDoc.setWorkflowStatusDisplay(document.getWorkflowStatusDisplay());
			wsDoc.setColor(document.getColor());

			setTemplateIntoWsDocument(document, wsDoc);

			wsDoc.setImmutable(document.getImmutable());

			setFolderIntoWsDocument(document, wsDoc);

			wsDoc.setIndexed(document.getIndexed());
			wsDoc.setVersion(document.getVersion());
			wsDoc.setFileVersion(document.getFileVersion());
			wsDoc.setPublisher(document.getPublisher());
			wsDoc.setPublisherId(document.getPublisherId());
			wsDoc.setCreator(document.getCreator());
			wsDoc.setCreatorId(document.getCreatorId());
			wsDoc.setStatus(document.getStatus());
			wsDoc.setType(document.getType());
			wsDoc.setLockUserId(document.getLockUserId());
			wsDoc.setFileName(document.getFileName());
			wsDoc.setFileSize(document.getFileSize());
			wsDoc.setDigest(document.getDigest());
			wsDoc.setDocRef(document.getDocRef());
			wsDoc.setDocRefType(document.getDocRefType());
			wsDoc.setLastModified(DateUtil.format(document.getLastModified()));
			wsDoc.setRating(document.getRating());
			wsDoc.setPages(document.getPages());
			wsDoc.setSigned(document.getSigned());
			wsDoc.setStamped(document.getStamped());
			wsDoc.setNature(document.getNature());
			wsDoc.setFormId(document.getFormId());
			wsDoc.setPasswordProtected(document.isPasswordProtected() ? 1 : 0);
			wsDoc.setOcrTemplateId(document.getOcrTemplateId());
			wsDoc.setOcrd(document.getOcrd());
			wsDoc.setBarcodeTemplateId(document.getBarcodeTemplateId());
			wsDoc.setBarcoded(document.getBarcoded());

			setDatesIntoWsDocument(document, wsDoc);

			wsDoc.setPublished(document.getPublished());

			// Populate the attributes
			setAttributesIntoWsDocument(document, wsDoc);

			String[] tags = new String[0];
			if (document.getTags() != null && document.getTags().size() > 0) {
				tags = new String[document.getTags().size()];
				List<String> docTags = new ArrayList<>(document.getTagsAsWords());
				if (docTags.size() > 0) {
					for (int j = 0; j < docTags.size(); j++) {
						tags[j] = docTags.get(j);
					}
				}
			}
			wsDoc.setTags(tags);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsDoc;
	}

	private static void setAttributesIntoWsDocument(AbstractDocument document, WSDocument wsDoc) {
		WSAttribute[] attributes = new WSAttribute[0];
		try {
			if (document.getAttributes() != null && document.getAttributes().size() > 0) {
				attributes = new WSAttribute[document.getAttributeNames().size()];
				int i = 0;
				for (String name : document.getAttributeNames()) {
					Attribute attr = document.getAttribute(name);

					WSAttribute attribute = new WSAttribute();
					attribute.setName(name);
					attribute.setMandatory(attr.getMandatory());
					attribute.setHidden(attr.getHidden());
					attribute.setReadonly(attr.getReadonly());
					attribute.setMultiple(attr.getMultiple());
					attribute.setParent(attr.getParent());
					attribute.setPosition(attr.getPosition());
					WSAttribute.setValue(attribute, attr.getValue());
					attribute.setSetId(attr.getSetId());
					attribute.setStringValues(attr.getStringValues());

					if (attr.getType() == Attribute.TYPE_USER || attr.getType() == Attribute.TYPE_FOLDER) {
						attribute.setIntValue(attr.getIntValue());
						attribute.setStringValue(attr.getStringValue());
					}

					attribute.setType(attr.getType());
					attribute.setDependsOn(attr.getDependsOn());
					attribute.setValidation(attr.getValidation());
					attribute.setInitialization(attr.getInitialization());
					attributes[i++] = attribute;
				}
			}
		} catch (Exception t) {
			// Nothing to do
		}
		wsDoc.setAttributes(attributes);
	}

	private static void setFolderIntoWsDocument(AbstractDocument document, WSDocument wsDocument) {
		if (document.getFolder() != null)
			wsDocument.setFolderId(document.getFolder().getId());
	}

	private static void setTemplateIntoWsDocument(AbstractDocument document, WSDocument wsDocument) {
		if (document.getTemplate() != null)
			wsDocument.setTemplateId(document.getTemplate().getId());
	}

	private static void setDatesIntoWsDocument(AbstractDocument document, WSDocument wsDocument) {
		String date = null;
		if (document.getDate() != null)
			date = DateUtil.format(document.getDate());
		wsDocument.setDate(date);
		date = null;
		if (document.getCreation() != null)
			date = DateUtil.format(document.getCreation());
		wsDocument.setCreation(date);
		date = null;
		if (document.getStartPublishing() != null)
			date = DateUtil.format(document.getStartPublishing());
		wsDocument.setStartPublishing(date);
		date = null;
		if (document.getStopPublishing() != null)
			date = DateUtil.format(document.getStopPublishing());
		wsDocument.setStopPublishing(date);
	}

	public static Document toDocument(WSDocument wsDoc) throws PersistenceException {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = fdao.findById(wsDoc.getFolderId());
		if (folder == null) {
			throw new PersistenceException("error - folder not found");
		}

		Document doc = new Document();
		doc.setFileName(wsDoc.getFileName());
		doc.setFolder(folder);
		doc.setComment(wsDoc.getComment());
		doc.setWorkflowStatus(wsDoc.getWorkflowStatus());
		doc.setWorkflowStatusDisplay(wsDoc.getWorkflowStatusDisplay());
		doc.setColor(wsDoc.getColor());
		doc.setLocale(LocaleUtil.toLocale(wsDoc.getLanguage()));

		Set<String> tagsSet = new TreeSet<>();
		if (wsDoc.getTags() != null) {
			for (int i = 0; i < wsDoc.getTags().length; i++) {
				tagsSet.add(wsDoc.getTags()[i]);
			}
		}
		doc.setTagsFromWords(tagsSet);

		setAttributesIntoDocument(wsDoc, doc);

		doc.setCustomId(wsDoc.getCustomId());
		doc.setLanguage(wsDoc.getLanguage());
		doc.setImmutable(wsDoc.getImmutable());
		if (wsDoc.getIndexed() != WSDocument.INDEX_INDEXED)
			doc.setIndexed(wsDoc.getIndexed());
		doc.setVersion(wsDoc.getVersion());
		doc.setFileVersion(wsDoc.getFileVersion());
		doc.setPages(wsDoc.getPages());
		doc.setNature(wsDoc.getNature());
		doc.setFormId(wsDoc.getFormId());

		setDatesIntoDocument(wsDoc, doc);

		doc.setPublisher(wsDoc.getPublisher());
		doc.setPublisherId(wsDoc.getPublisherId());
		doc.setCreator(wsDoc.getCreator());
		doc.setCreatorId(wsDoc.getCreatorId());
		doc.setStatus(wsDoc.getStatus());
		doc.setType(wsDoc.getType());
		doc.setLockUserId(wsDoc.getLockUserId());
		doc.setFileSize(wsDoc.getFileSize());
		doc.setDigest(wsDoc.getDigest());
		doc.setDocRef(wsDoc.getDocRef());
		doc.setDocRefType(wsDoc.getDocRefType());
		if (wsDoc.getRating() != null)
			doc.setRating(wsDoc.getRating());
		doc.setPublished(wsDoc.getPublished());

		doc.setOcrTemplateId(wsDoc.getOcrTemplateId());
		doc.setBarcodeTemplateId(wsDoc.getBarcodeTemplateId());

		doc.setSigned(wsDoc.getSigned());
		doc.setBarcoded(wsDoc.getBarcoded());

		return doc;
	}

	private static void setAttributesIntoDocument(WSDocument wsDoc, Document doc) throws PersistenceException {
		Template template = null;
		Map<String, Attribute> attrs = new HashMap<>();
		if (wsDoc.getTemplateId() != null) {
			TemplateDAO templDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			template = templDao.findById(wsDoc.getTemplateId());
			doc.setTemplate(template);
			if (template != null) {
				doc.setTemplateId(template.getId());
				if (wsDoc.getAttributes() != null && wsDoc.getAttributes().length > 0) {
					for (int i = 0; i < wsDoc.getAttributes().length; i++) {
						Attribute att = new Attribute();
						att.setMandatory(wsDoc.getAttributes()[i].getMandatory());
						att.setHidden(wsDoc.getAttributes()[i].getHidden());
						att.setReadonly(wsDoc.getAttributes()[i].getReadonly());
						att.setMultiple(wsDoc.getAttributes()[i].getMultiple());
						att.setParent(wsDoc.getAttributes()[i].getParent());
						att.setDependsOn(wsDoc.getAttributes()[i].getDependsOn());
						att.setPosition(wsDoc.getAttributes()[i].getPosition());
						att.setIntValue(wsDoc.getAttributes()[i].getIntValue());
						att.setStringValue(wsDoc.getAttributes()[i].getStringValue());
						att.setDoubleValue(wsDoc.getAttributes()[i].getDoubleValue());
						att.setDateValue(convertStringToDate(wsDoc.getAttributes()[i].getDateValue()));
						att.setSetId(wsDoc.getAttributes()[i].getSetId());
						att.setType(wsDoc.getAttributes()[i].getType());
						att.setDependsOn(wsDoc.getAttributes()[i].getDependsOn());
						att.setValidation(wsDoc.getAttributes()[i].getValidation());
						att.setInitialization(wsDoc.getAttributes()[i].getInitialization());

						attrs.put(wsDoc.getAttributes()[i].getName(), att);
					}
				}
			}
		}
		doc.setAttributes(attrs);
	}

	private static void setDatesIntoDocument(WSDocument wsDocument, Document document) {
		Date newdate = null;
		if (StringUtils.isNotEmpty(wsDocument.getDate()))
			newdate = convertStringToDate(wsDocument.getDate());
		document.setDate(newdate);

		Date creationDate = null;
		if (StringUtils.isNotEmpty(wsDocument.getCreation()))
			creationDate = convertStringToDate(wsDocument.getCreation());
		document.setCreation(creationDate);

		if (StringUtils.isNotEmpty(wsDocument.getStartPublishing()))
			document.setStartPublishing(convertStringToDate(wsDocument.getStartPublishing()));
		if (StringUtils.isNotEmpty(wsDocument.getStopPublishing()))
			document.setStopPublishing(convertStringToDate(wsDocument.getStopPublishing()));
	}

	public static Date convertStringToDate(String date) {
		if (StringUtils.isEmpty(date))
			return null;

		try {
			return DateUtils.parseDate(date, "yyyy-MM-dd HH:mm:ss.SSS Z", "yyyy-MM-dd HH:mm:ss.SS Z",
					"yyyy-MM-dd HH:mm:ss Z", "yyyy-MM-dd");
		} catch (ParseException e) {
			log.error("Unparseable date {}", date);
		}

		return null;
	}

	public static WSAttributeSet toWSAttributeSet(AttributeSet attributeSet) {
		WSAttributeSet wsAttributeSet = new WSAttributeSet();

		try {
			wsAttributeSet.setId(attributeSet.getId());
			wsAttributeSet.setName(attributeSet.getName());
			wsAttributeSet.setDescription(attributeSet.getDescription());
			wsAttributeSet.setLastModified(DateUtil.format(attributeSet.getLastModified()));

			// Populate extended attributes
			WSAttribute[] attributes = new WSAttribute[0];
			if (attributeSet.getAttributes() != null && attributeSet.getAttributes().size() > 0) {
				attributes = new WSAttribute[attributeSet.getAttributeNames().size()];
				int i = 0;
				for (String name : attributeSet.getAttributeNames()) {
					Attribute attr = attributeSet.getAttribute(name);
					WSAttribute attribute = new WSAttribute();
					attribute.setName(name);
					attribute.setLabel(attr.getLabel());
					attribute.setMandatory(attr.getMandatory());
					attribute.setHidden(attr.getHidden());
					attribute.setReadonly(attr.getReadonly());
					attribute.setMultiple(attr.getMultiple());
					attribute.setParent(attr.getParent());
					attribute.setPosition(attr.getPosition());
					attribute.setStringValues(attr.getStringValues());
					attribute.setStringValue(attr.getStringValue());
					attribute.setIntValue(attr.getIntValue());
					attribute.setDoubleValue(attr.getDoubleValue());
					attribute.setDateValue(DateUtil.format(attr.getDateValue()));
					attribute.setEditor(attr.getEditor());
					attribute.setSetId(attr.getSetId());
					attribute.setType(attr.getType());
					attribute.setDependsOn(attr.getDependsOn());
					attribute.setValidation(attr.getValidation());
					attribute.setInitialization(attr.getInitialization());
					attributes[i++] = attribute;
				}
				wsAttributeSet.setAttributes(attributes);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsAttributeSet;
	}

	public static AttributeSet toAttributeSet(WSAttributeSet wsSet) {
		AttributeSet set = new AttributeSet();

		try {
			set.setId(wsSet.getId());
			set.setName(wsSet.getName());
			set.setDescription(wsSet.getDescription());

			Map<String, Attribute> attributes = null;
			if (wsSet.getAttributes() != null && wsSet.getAttributes().length > 0) {
				set.getAttributes().clear();
				attributes = new HashMap<>();
				for (int i = 0; i < wsSet.getAttributes().length; i++) {
					Attribute att = new Attribute();
					att.setLabel(wsSet.getAttributes()[i].getLabel());
					att.setMandatory(wsSet.getAttributes()[i].getMandatory());
					att.setHidden(wsSet.getAttributes()[i].getHidden());
					att.setReadonly(wsSet.getAttributes()[i].getReadonly());
					att.setMultiple(wsSet.getAttributes()[i].getMultiple());
					att.setParent(wsSet.getAttributes()[i].getParent());
					att.setDependsOn(wsSet.getAttributes()[i].getDependsOn());
					att.setPosition(wsSet.getAttributes()[i].getPosition());
					att.setStringValue(wsSet.getAttributes()[i].getStringValue());
					att.setIntValue(wsSet.getAttributes()[i].getIntValue());
					att.setDoubleValue(wsSet.getAttributes()[i].getDoubleValue());
					att.setDateValue(convertStringToDate(wsSet.getAttributes()[i].getStringValue()));
					att.setEditor(wsSet.getAttributes()[i].getEditor());
					att.setSetId(wsSet.getAttributes()[i].getSetId());
					att.setType(wsSet.getAttributes()[i].getType());
					att.setDependsOn(wsSet.getAttributes()[i].getDependsOn());
					att.setValidation(wsSet.getAttributes()[i].getValidation());
					att.setInitialization(wsSet.getAttributes()[i].getInitialization());
					attributes.put(wsSet.getAttributes()[i].getName(), att);
				}
				set.setAttributes(attributes);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return set;
	}

	public static Template toTemplate(WSTemplate wsTemplate) {
		Template template = new Template();

		try {
			template.setId(wsTemplate.getId());
			template.setName(wsTemplate.getName());
			template.setDescription(wsTemplate.getDescription());
			template.setValidation(wsTemplate.getValidation());

			Map<String, Attribute> attributes = null;
			if (wsTemplate.getAttributes() != null && wsTemplate.getAttributes().length > 0) {
				template.getAttributes().clear();
				attributes = new HashMap<>();
				for (int i = 0; i < wsTemplate.getAttributes().length; i++) {
					Attribute att = new Attribute();
					att.setLabel(wsTemplate.getAttributes()[i].getLabel());
					att.setHidden(wsTemplate.getAttributes()[i].getHidden());
					att.setReadonly(wsTemplate.getAttributes()[i].getReadonly());
					att.setMultiple(wsTemplate.getAttributes()[i].getMultiple());
					att.setParent(wsTemplate.getAttributes()[i].getParent());
					att.setDependsOn(wsTemplate.getAttributes()[i].getDependsOn());
					att.setPosition(wsTemplate.getAttributes()[i].getPosition());
					att.setType(wsTemplate.getAttributes()[i].getType());
					att.setStringValue(wsTemplate.getAttributes()[i].getStringValue());
					att.setIntValue(wsTemplate.getAttributes()[i].getIntValue());
					att.setDoubleValue(wsTemplate.getAttributes()[i].getDoubleValue());
					att.setDateValue(convertStringToDate(wsTemplate.getAttributes()[i].getStringValue()));
					att.setEditor(wsTemplate.getAttributes()[i].getEditor());
					att.setSetId(wsTemplate.getAttributes()[i].getSetId());
					att.setDependsOn(wsTemplate.getAttributes()[i].getDependsOn());
					att.setValidation(wsTemplate.getAttributes()[i].getValidation());
					att.setInitialization(wsTemplate.getAttributes()[i].getInitialization());
					attributes.put(wsTemplate.getAttributes()[i].getName(), att);
				}
				template.setAttributes(attributes);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return template;
	}

	public static WSTemplate toWSTemplate(Template template) {
		WSTemplate wsTemplate = new WSTemplate();

		try {
			wsTemplate.setId(template.getId());
			wsTemplate.setName(template.getName());
			wsTemplate.setDescription(template.getDescription());
			wsTemplate.setValidation(template.getValidation());
			wsTemplate.setLastModified(DateUtil.format(template.getLastModified()));

			TemplateDAO templateDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			templateDao.initialize(template);
			wsTemplate.setDocsCount(templateDao.countDocs(template.getId()));

			// Populate extended attributes
			WSAttribute[] attributes = new WSAttribute[0];
			if (template.getAttributes() != null && template.getAttributes().size() > 0) {
				attributes = new WSAttribute[template.getAttributeNames().size()];
				int i = 0;
				for (String name : template.getAttributeNames()) {
					Attribute attr = template.getAttribute(name);
					WSAttribute attribute = new WSAttribute();
					attribute.setName(name);
					attribute.setLabel(attr.getLabel());
					attribute.setMandatory(attr.getMandatory());
					attribute.setHidden(attr.getHidden());
					attribute.setReadonly(attr.getReadonly());
					attribute.setMultiple(attr.getMultiple());
					attribute.setParent(attr.getParent());
					attribute.setPosition(attr.getPosition());
					attribute.setStringValue(attr.getStringValue());
					attribute.setStringValues(attr.getStringValues());
					attribute.setIntValue(attr.getIntValue());
					attribute.setDoubleValue(attr.getDoubleValue());
					attribute.setDateValue(DateUtil.format(attr.getDateValue()));
					attribute.setEditor(attr.getEditor());
					attribute.setSetId(attr.getSetId());
					attribute.setType(attr.getType());
					attribute.setDependsOn(attr.getDependsOn());
					attribute.setValidation(attr.getValidation());
					attribute.setInitialization(attr.getInitialization());
					attributes[i++] = attribute;
				}
				wsTemplate.setAttributes(attributes);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsTemplate;
	}
}