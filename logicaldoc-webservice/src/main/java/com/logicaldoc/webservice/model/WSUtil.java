package com.logicaldoc.webservice.model;

import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSet;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
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

			// Set the tags
			wsDoc.setTags(document.getTags().stream().map(Tag::getTag).collect(Collectors.toList()));
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsDoc;
	}

	private static void setAttributesIntoWsDocument(AbstractDocument document, WSDocument wsDoc) {
		List<WSAttribute> attributes = new ArrayList<>();
		try {
			if (MapUtils.isNotEmpty(document.getAttributes())) {
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

					if (attr.getType() == Attribute.TYPE_USER || attr.getType() == Attribute.TYPE_FOLDER
							|| attr.getType() == Attribute.TYPE_DOCUMENT) {
						attribute.setIntValue(attr.getIntValue());
						attribute.setStringValue(attr.getStringValue());
					}

					attribute.setType(attr.getType());
					attribute.setDependsOn(attr.getDependsOn());
					attribute.setValidation(attr.getValidation());
					attribute.setInitialization(attr.getInitialization());
					attributes.add(attribute);
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
		FolderDAO fdao = Context.get().getBean(FolderDAO.class);
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

		if (CollectionUtils.isNotEmpty(wsDoc.getTags()))
			doc.setTagsFromWords(new HashSet<>(wsDoc.getTags()));

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
			TemplateDAO templDao = Context.get().getBean(TemplateDAO.class);
			template = templDao.findById(wsDoc.getTemplateId());
			doc.setTemplate(template);
			if (template != null) {
				doc.setTemplateId(template.getId());
				if (CollectionUtils.isNotEmpty(wsDoc.getAttributes())) {
					for (WSAttribute wsAtt : wsDoc.getAttributes()) {
						Attribute att = new Attribute();
						att.setMandatory(wsAtt.getMandatory());
						att.setHidden(wsAtt.getHidden());
						att.setReadonly(wsAtt.getReadonly());
						att.setMultiple(wsAtt.getMultiple());
						att.setParent(wsAtt.getParent());
						att.setDependsOn(wsAtt.getDependsOn());
						att.setPosition(wsAtt.getPosition());
						att.setIntValue(wsAtt.getIntValue());
						att.setStringValue(wsAtt.getStringValue());
						att.setDoubleValue(wsAtt.getDoubleValue());
						att.setDateValue(convertStringToDate(wsAtt.getDateValue()));
						att.setSetId(wsAtt.getSetId());
						att.setType(wsAtt.getType());
						att.setDependsOn(wsAtt.getDependsOn());
						att.setValidation(wsAtt.getValidation());
						att.setInitialization(wsAtt.getInitialization());

						attrs.put(wsAtt.getName(), att);
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

	public static String convertDateToString(Date date) {
		if (date == null)
			return null;

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		try {
			return df.format(date);
		} catch (Exception e) {
			df = new SimpleDateFormat("yyyy-MM-dd");
			return df.format(date);
		}
	}

	public static Date convertStringToDate(String date) {
		if (StringUtils.isEmpty(date))
			return null;

		try {
			return DateUtils.parseDate(date, "yyyy-MM-dd HH:mm:ss.SSS Z", "yyyy-MM-dd HH:mm:ss.SS Z",
					"yyyy-MM-dd HH:mm:ss Z", "yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd");
		} catch (Exception e) {
			log.error("Unparseable date {}", date);
			log.error(e.getMessage(), e);
		}

		return null;
	}

	public static WSAttributeSet toWSAttributeSet(AttributeSet attributeSet) throws PersistenceException {
		WSAttributeSet wsAttributeSet = new WSAttributeSet();

		wsAttributeSet.setId(attributeSet.getId());
		wsAttributeSet.setName(attributeSet.getName());
		wsAttributeSet.setDescription(attributeSet.getDescription());
		wsAttributeSet.setLastModified(DateUtil.format(attributeSet.getLastModified()));

		AttributeSetDAO setDao = Context.get().getBean(AttributeSetDAO.class);
		setDao.initialize(attributeSet);

		// Populate extended attributes
		List<WSAttribute> wsAttributes;
		if (CollectionUtils.isNotEmpty(wsAttributeSet.getAttributes())) {
			wsAttributes = new ArrayList<>();
			for (String name : attributeSet.getAttributeNames()) {
				Attribute attr = attributeSet.getAttribute(name);
				WSAttribute wsAttribute = new WSAttribute();
				wsAttribute.setName(name);
				wsAttribute.setLabel(attr.getLabel());
				wsAttribute.setMandatory(attr.getMandatory());
				wsAttribute.setHidden(attr.getHidden());
				wsAttribute.setReadonly(attr.getReadonly());
				wsAttribute.setMultiple(attr.getMultiple());
				wsAttribute.setParent(attr.getParent());
				wsAttribute.setPosition(attr.getPosition());
				wsAttribute.setStringValues(attr.getStringValues());
				wsAttribute.setStringValue(attr.getStringValue());
				wsAttribute.setIntValue(attr.getIntValue());
				wsAttribute.setDoubleValue(attr.getDoubleValue());
				wsAttribute.setDateValue(DateUtil.format(attr.getDateValue()));
				wsAttribute.setEditor(attr.getEditor());
				wsAttribute.setSetId(attr.getSetId());
				wsAttribute.setType(attr.getType());
				wsAttribute.setDependsOn(attr.getDependsOn());
				wsAttribute.setValidation(attr.getValidation());
				wsAttribute.setInitialization(attr.getInitialization());
				wsAttributes.add(wsAttribute);
			}
			wsAttributeSet.setAttributes(wsAttributes);
		}

		return wsAttributeSet;
	}

	public static AttributeSet toAttributeSet(WSAttributeSet wsSet) {
		AttributeSet set = new AttributeSet();

		set.setId(wsSet.getId());
		set.setName(wsSet.getName());
		set.setDescription(wsSet.getDescription());

		Map<String, Attribute> attributes = null;
		if (CollectionUtils.isNotEmpty(wsSet.getAttributes())) {
			set.getAttributes().clear();
			attributes = new HashMap<>();
			for (WSAttribute wsAtt : wsSet.getAttributes()) {
				Attribute att = new Attribute();
				att.setLabel(wsAtt.getLabel());
				att.setMandatory(wsAtt.getMandatory());
				att.setHidden(wsAtt.getHidden());
				att.setReadonly(wsAtt.getReadonly());
				att.setMultiple(wsAtt.getMultiple());
				att.setParent(wsAtt.getParent());
				att.setDependsOn(wsAtt.getDependsOn());
				att.setPosition(wsAtt.getPosition());
				att.setStringValue(wsAtt.getStringValue());
				att.setIntValue(wsAtt.getIntValue());
				att.setDoubleValue(wsAtt.getDoubleValue());
				att.setDateValue(convertStringToDate(wsAtt.getStringValue()));
				att.setEditor(wsAtt.getEditor());
				att.setSetId(wsAtt.getSetId());
				att.setType(wsAtt.getType());
				att.setDependsOn(wsAtt.getDependsOn());
				att.setValidation(wsAtt.getValidation());
				att.setInitialization(wsAtt.getInitialization());
				attributes.put(wsAtt.getName(), att);
			}
			set.setAttributes(attributes);
		}

		return set;
	}

	public static Template toTemplate(WSTemplate wsTemplate) {
		Template template = new Template();

		template.setId(wsTemplate.getId());
		template.setName(wsTemplate.getName());
		template.setDescription(wsTemplate.getDescription());
		template.setValidation(wsTemplate.getValidation());

		Map<String, Attribute> attributes = null;
		if (CollectionUtils.isNotEmpty(wsTemplate.getAttributes())) {
			template.getAttributes().clear();
			attributes = new HashMap<>();
			for (WSAttribute wsAtt : wsTemplate.getAttributes()) {
				Attribute att = new Attribute();
				att.setLabel(wsAtt.getLabel());
				att.setHidden(wsAtt.getHidden());
				att.setReadonly(wsAtt.getReadonly());
				att.setMultiple(wsAtt.getMultiple());
				att.setParent(wsAtt.getParent());
				att.setDependsOn(wsAtt.getDependsOn());
				att.setPosition(wsAtt.getPosition());
				att.setType(wsAtt.getType());
				att.setStringValue(wsAtt.getStringValue());
				att.setIntValue(wsAtt.getIntValue());
				att.setDoubleValue(wsAtt.getDoubleValue());
				att.setDateValue(convertStringToDate(wsAtt.getStringValue()));
				att.setEditor(wsAtt.getEditor());
				att.setSetId(wsAtt.getSetId());
				att.setDependsOn(wsAtt.getDependsOn());
				att.setValidation(wsAtt.getValidation());
				att.setInitialization(wsAtt.getInitialization());
				attributes.put(wsAtt.getName(), att);
			}
			template.setAttributes(attributes);
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

			TemplateDAO templateDao = Context.get().getBean(TemplateDAO.class);
			templateDao.initialize(template);
			wsTemplate.setDocsCount(templateDao.countDocs(template.getId()));

			// Populate extended attributes
			List<WSAttribute> attributes;
			if (MapUtils.isNotEmpty(template.getAttributes())) {
				attributes = new ArrayList<>();
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
					attributes.add(attribute);
				}
				wsTemplate.setAttributes(attributes);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return wsTemplate;
	}

	public static WSAccessControlEntry toWSAccessControlEntry(AccessControlEntry ace) throws PersistenceException {
		WSAccessControlEntry wsAce = new WSAccessControlEntry();
		try {
			BeanUtils.copyProperties(wsAce, ace);
		} catch (IllegalAccessException | InvocationTargetException e) {
			throw new PersistenceException(e.getMessage(), e);
		}

		GroupDAO groupDao = Context.get().getBean(GroupDAO.class);
		Group group = groupDao.findById(ace.getGroupId());
		if (group.getName().startsWith("_user_"))
			wsAce.setUserId(Long.parseLong(group.getName().substring(group.getName().lastIndexOf('_') + 1)));

		return wsAce;
	}

	public static AccessControlEntry toAccessControlEntry(WSAccessControlEntry wsAce) throws PersistenceException {
		AccessControlEntry ace = new AccessControlEntry();
		try {
			BeanUtils.copyProperties(ace, wsAce);
		} catch (IllegalAccessException | InvocationTargetException e) {
			throw new PersistenceException(e.getMessage(), e);
		}

		if (wsAce.getUserId() != 0L) {
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			User user = userDao.findById(wsAce.getUserId());
			userDao.initialize(user);
			ace.setGroupId(user.getUserGroup().getId());
		}

		return ace;
	}
}