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

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;

public class WSUtil {
	public static WSDocument toWSDocument(AbstractDocument document) {
		WSDocument wsDoc = new WSDocument();

		try {
			wsDoc.setId(document.getId());
			wsDoc.setCustomId(document.getCustomId());
			wsDoc.setLanguage(document.getLanguage());
			wsDoc.setComment(document.getComment());
			wsDoc.setWorkflowStatus(document.getWorkflowStatus());
			if (document.getTemplate() != null)
				wsDoc.setTemplateId(document.getTemplate().getId());
			wsDoc.setImmutable(document.getImmutable());
			if (document.getFolder() != null)
				wsDoc.setFolderId(document.getFolder().getId());
			if (document.getIndexed() != WSDocument.INDEX_INDEXED)
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
						attribute.setPosition(attr.getPosition());
						attribute.setType(attr.getType());
						attribute.setValue(attr.getValue());
						attribute.setSetId(attr.getSetId());

						if (attr.getType() == Attribute.TYPE_USER) {
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
			e.printStackTrace();
		}

		return wsDoc;
	}

	public static Document toDocument(WSDocument wsDoc) throws Exception {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = fdao.findById(wsDoc.getFolderId());
		if (folder == null) {
			throw new Exception("error - folder not found");
		}
		fdao.initialize(folder);

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
						att.setPosition(wsDoc.getAttributes()[i].getPosition());
						att.setIntValue(wsDoc.getAttributes()[i].getIntValue());
						att.setStringValue(wsDoc.getAttributes()[i].getStringValue());
						att.setDoubleValue(wsDoc.getAttributes()[i].getDoubleValue());
						att.setDateValue(convertStringToDate(wsDoc.getAttributes()[i].getDateValue()));
						att.setType(wsDoc.getAttributes()[i].getType());
						att.setSetId(wsDoc.getAttributes()[i].getSetId());

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
}