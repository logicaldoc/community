package com.logicaldoc.webservice.model;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

public class WSUtil {
	protected static Logger log = LoggerFactory.getLogger(WSUtil.class);

	public static WSDocument toWSDocument(AbstractDocument document) {
		WSDocument wsDoc = new WSDocument();

		try {
			wsDoc.setId(document.getId());
			wsDoc.setCustomId(document.getCustomId());
			wsDoc.setLanguage(document.getLanguage());
			wsDoc.setComment(document.getComment());
			wsDoc.setWorkflowStatus(document.getWorkflowStatus());
			wsDoc.setWorkflowStatusDisplay(document.getWorkflowStatusDisplay());
			if (document.getTemplate() != null)
				wsDoc.setTemplateId(document.getTemplate().getId());
			wsDoc.setImmutable(document.getImmutable());
			if (document.getFolder() != null)
				wsDoc.setFolderId(document.getFolder().getId());
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
			wsDoc.setLastModified(convertDateToString(document.getLastModified()));
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

			String date = null;
			if (document.getDate() != null)
				date = convertDateToString(document.getDate());
			wsDoc.setDate(date);
			date = null;
			if (document.getCreation() != null)
				date = convertDateToString(document.getCreation());
			wsDoc.setCreation(date);
			date = null;
			if (document.getStartPublishing() != null)
				date = convertDateToString(document.getStartPublishing());
			wsDoc.setStartPublishing(date);
			date = null;
			if (document.getStopPublishing() != null)
				date = convertDateToString(document.getStopPublishing());
			wsDoc.setStopPublishing(date);
			wsDoc.setPublished(document.getPublished());

			// Populate the attributes
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
						attribute.setMultiple(attr.getMultiple());
						attribute.setParent(attr.getParent());
						attribute.setPosition(attr.getPosition());
						attribute.setValue(attr.getValue());
						attribute.setSetId(attr.getSetId());
						attribute.setStringValues(attr.getStringValues());

						if (attr.getType() == Attribute.TYPE_USER || attr.getType() == Attribute.TYPE_FOLDER) {
							attribute.setIntValue(attr.getIntValue());
							attribute.setStringValue(attr.getStringValue());
						}

						attribute.setType(attr.getType());
						attributes[i++] = attribute;
					}
				}
			} catch (Throwable t) {
			}
			wsDoc.setAttributes(attributes);

			String[] tags = new String[0];
			if (document.getTags() != null && document.getTags().size() > 0) {
				tags = new String[document.getTags().size()];
				List<String> docTags = new ArrayList<String>(document.getTagsAsWords());
				if (docTags != null && docTags.size() > 0) {
					for (int j = 0; j < docTags.size(); j++) {
						tags[j] = docTags.get(j);
					}
				}
			}
			wsDoc.setTags(tags);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return wsDoc;
	}

	public static Document toDocument(WSDocument wsDoc) throws Exception {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = fdao.findById(wsDoc.getFolderId());
		if (folder == null) {
			throw new Exception("error - folder not found");
		}
		// fdao.initialize(folder);

		Set<String> tagsSet = new TreeSet<String>();
		if (wsDoc.getTags() != null) {
			for (int i = 0; i < wsDoc.getTags().length; i++) {
				tagsSet.add(wsDoc.getTags()[i]);
			}
		}

		Template template = null;
		Map<String, Attribute> attrs = new HashMap<String, Attribute>();
		if (wsDoc.getTemplateId() != null) {
			TemplateDAO templDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			template = templDao.findById(wsDoc.getTemplateId());
			if (template != null) {
				if (wsDoc.getAttributes() != null && wsDoc.getAttributes().length > 0) {
					for (int i = 0; i < wsDoc.getAttributes().length; i++) {
						Attribute att = new Attribute();
						att.setMandatory(wsDoc.getAttributes()[i].getMandatory());
						att.setHidden(wsDoc.getAttributes()[i].getHidden());
						att.setMultiple(wsDoc.getAttributes()[i].getMultiple());
						att.setParent(wsDoc.getAttributes()[i].getParent());
						att.setPosition(wsDoc.getAttributes()[i].getPosition());
						att.setIntValue(wsDoc.getAttributes()[i].getIntValue());
						att.setStringValue(wsDoc.getAttributes()[i].getStringValue());
						att.setDoubleValue(wsDoc.getAttributes()[i].getDoubleValue());
						att.setDateValue(convertStringToDate(wsDoc.getAttributes()[i].getDateValue()));
						att.setSetId(wsDoc.getAttributes()[i].getSetId());
						att.setType(wsDoc.getAttributes()[i].getType());

						attrs.put(wsDoc.getAttributes()[i].getName(), att);
					}
				}
			}
		}

		Document doc = new Document();
		doc.setFileName(wsDoc.getFileName());
		doc.setFolder(folder);
		doc.setComment(wsDoc.getComment());
		doc.setWorkflowStatus(wsDoc.getWorkflowStatus());
		doc.setWorkflowStatusDisplay(wsDoc.getWorkflowStatusDisplay());
		doc.setLocale(LocaleUtil.toLocale(wsDoc.getLanguage()));
		doc.setTagsFromWords(tagsSet);
		doc.setTemplate(template);
		if (template != null)
			doc.setTemplateId(template.getId());
		doc.setAttributes(attrs);
		doc.setCustomId(wsDoc.getCustomId());
		doc.setLanguage(wsDoc.getLanguage());
		doc.setImmutable(wsDoc.getImmutable());
		if (wsDoc.getIndexed() != WSDocument.INDEX_INDEXED)
			doc.setIndexed(wsDoc.getIndexed());
		doc.setVersion(wsDoc.getVersion());
		doc.setFileVersion(wsDoc.getFileVersion());
		Date newdate = null;
		if (StringUtils.isNotEmpty(wsDoc.getDate()))
			newdate = convertStringToDate(wsDoc.getDate());
		doc.setDate(newdate);
		doc.setPages(wsDoc.getPages());
		doc.setNature(wsDoc.getNature());
		doc.setFormId(wsDoc.getFormId());

		Date creationDate = null;
		if (StringUtils.isNotEmpty(wsDoc.getCreation()))
			creationDate = convertStringToDate(wsDoc.getCreation());
		doc.setCreation(creationDate);

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
		if (StringUtils.isNotEmpty(wsDoc.getStartPublishing()))
			doc.setStartPublishing(convertStringToDate(wsDoc.getStartPublishing()));
		if (StringUtils.isNotEmpty(wsDoc.getStopPublishing()))
			doc.setStopPublishing(convertStringToDate(wsDoc.getStopPublishing()));
		doc.setOcrTemplateId(wsDoc.getOcrTemplateId());
		doc.setBarcodeTemplateId(wsDoc.getBarcodeTemplateId());

		return doc;
	}

	public static String convertDateToString(Date date) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		try {
			return df.format(date);
		} catch (Exception e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			try {
				return df.format(date);
			} catch (Exception e1) {
			}
		}
		return null;
	}

	public static Date convertStringToDate(String date) {
		if (StringUtils.isEmpty(date))
			return null;

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		try {
			return df.parse(date);
		} catch (ParseException e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			try {
				return df.parse(date);
			} catch (ParseException e1) {
			}
		}
		return null;
	}

	public static WSAttributeSet toWSAttributeSet(AttributeSet attributeSet) {
		WSAttributeSet wsAttributeSet = new WSAttributeSet();

		try {
			wsAttributeSet.setId(attributeSet.getId());
			wsAttributeSet.setName(attributeSet.getName());
			wsAttributeSet.setDescription(attributeSet.getDescription());
			wsAttributeSet.setLastModified(convertDateToString(attributeSet.getLastModified()));

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
					attribute.setMultiple(attr.getMultiple());
					attribute.setParent(attr.getParent());
					attribute.setPosition(attr.getPosition());
					attribute.setStringValues(attr.getStringValues());
					attribute.setStringValue(attr.getStringValue());
					attribute.setIntValue(attr.getIntValue());
					attribute.setDoubleValue(attr.getDoubleValue());
					attribute.setDateValue(convertDateToString(attr.getDateValue()));
					attribute.setEditor(attr.getEditor());
					attribute.setSetId(attr.getSetId());
					attribute.setType(attr.getType());
					attributes[i++] = attribute;
				}
				wsAttributeSet.setAttributes(attributes);
			}
		} catch (Throwable e) {
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
				attributes = new HashMap<String, Attribute>();
				for (int i = 0; i < wsSet.getAttributes().length; i++) {
					Attribute att = new Attribute();
					att.setLabel(wsSet.getAttributes()[i].getLabel());
					att.setMandatory(wsSet.getAttributes()[i].getMandatory());
					att.setHidden(wsSet.getAttributes()[i].getHidden());
					att.setMultiple(wsSet.getAttributes()[i].getMultiple());
					att.setParent(wsSet.getAttributes()[i].getParent());
					att.setPosition(wsSet.getAttributes()[i].getPosition());
					att.setStringValue(wsSet.getAttributes()[i].getStringValue());
					att.setIntValue(wsSet.getAttributes()[i].getIntValue());
					att.setDoubleValue(wsSet.getAttributes()[i].getDoubleValue());
					att.setDateValue(convertStringToDate(wsSet.getAttributes()[i].getStringValue()));
					att.setEditor(wsSet.getAttributes()[i].getEditor());
					att.setSetId(wsSet.getAttributes()[i].getSetId());
					att.setType(wsSet.getAttributes()[i].getType());
					attributes.put(wsSet.getAttributes()[i].getName(), att);
				}
				set.setAttributes(attributes);
			}
		} catch (Throwable e) {
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

			Map<String, Attribute> attributes = null;
			if (wsTemplate.getAttributes() != null && wsTemplate.getAttributes().length > 0) {
				template.getAttributes().clear();
				attributes = new HashMap<String, Attribute>();
				for (int i = 0; i < wsTemplate.getAttributes().length; i++) {
					Attribute att = new Attribute();
					att.setLabel(wsTemplate.getAttributes()[i].getLabel());
					att.setHidden(wsTemplate.getAttributes()[i].getHidden());
					att.setMultiple(wsTemplate.getAttributes()[i].getMultiple());
					att.setParent(wsTemplate.getAttributes()[i].getParent());
					att.setPosition(wsTemplate.getAttributes()[i].getPosition());
					att.setType(wsTemplate.getAttributes()[i].getType());
					att.setStringValue(wsTemplate.getAttributes()[i].getStringValue());
					att.setIntValue(wsTemplate.getAttributes()[i].getIntValue());
					att.setDoubleValue(wsTemplate.getAttributes()[i].getDoubleValue());
					att.setDateValue(convertStringToDate(wsTemplate.getAttributes()[i].getStringValue()));
					att.setEditor(wsTemplate.getAttributes()[i].getEditor());
					att.setSetId(wsTemplate.getAttributes()[i].getSetId());
					attributes.put(wsTemplate.getAttributes()[i].getName(), att);
				}
				template.setAttributes(attributes);
			}
		} catch (Throwable e) {
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
			wsTemplate.setLastModified(convertDateToString(template.getLastModified()));

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
					attribute.setMultiple(attr.getMultiple());
					attribute.setParent(attr.getParent());
					attribute.setPosition(attr.getPosition());
					attribute.setStringValue(attr.getStringValue());
					attribute.setStringValues(attr.getStringValues());
					attribute.setIntValue(attr.getIntValue());
					attribute.setDoubleValue(attr.getDoubleValue());
					attribute.setDateValue(convertDateToString(attr.getDateValue()));
					attribute.setEditor(attr.getEditor());
					attribute.setSetId(attr.getSetId());
					attribute.setType(attr.getType());
					attributes[i++] = attribute;
				}
				wsTemplate.setAttributes(attributes);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		return wsTemplate;
	}
}