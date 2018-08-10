package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Folder. Useful class to create repository Folders.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@XmlRootElement(name = "folder")
@XmlType(name = "WSFolder")
public class WSFolder implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(documented = false)
	protected static Logger log = LoggerFactory.getLogger(WSFolder.class);

	@WSDoc(description = "unique identifier of the folder")
	private long id = 0;

	private String name = "";

	@WSDoc(description = "identifier of the parent folder")
	private long parentId = 0;

	private String description = "";

	@WSDoc(description = "the last modified date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String lastModified;

	@WSDoc(description = "<b>0</b> = Folder, <b>1</b> = Workspace")
	private int type = Folder.TYPE_DEFAULT;

	@WSDoc(description = "template assigned to folder", required = false)
	private Long templateId;

	@WSDoc(description = "<b>0</b> = the template is unlocked, <b>1</b> = the template is locked ")
	private int templateLocked = 0;

	@WSDoc(description = "the creation date (format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd')", required = false)
	private String creation;

	@WSDoc(description = "who created the folder", required = false)
	private String creator;

	private int position = 1;

	@WSDoc(description = "<b>0</b> = visible, <b>1</b> = hidden")
	private int hidden = 0;

	@WSDoc(description = "the referenced folder, used in case of folder alias", required = false)
	private Long foldRef = null;

	@WSDoc(description = "array of attributes", required = false)
	private WSAttribute[] attributes = new WSAttribute[0];

	@WSDoc(description = "the storage to use for new files. Valid only in case of workspace.", required = false)
	private Integer storage = null;

	@WSDoc(required = false, description = "tags applied to the document")
	private String[] tags = new String[0];

	public void addAttribute(WSAttribute att) {
		List<WSAttribute> buf = (List<WSAttribute>) Arrays.asList(getAttributes());
		buf.add(att);
		setAttributes(buf.toArray(new WSAttribute[0]));
	}

	public void addTag(String tag) {
		if (tags == null)
			tags = new String[0];
		List<String> buf = new ArrayList<String>();
		for (String tmp : tags)
			buf.add(tmp);
		if (!buf.contains(tag))
			buf.add(tag);
		setTags(buf.toArray(new String[0]));
	}

	public Collection<String> listAttributeNames() {
		List<String> names = new ArrayList<String>();
		for (WSAttribute att : getAttributes()) {
			names.add(att.getName());
		}
		return names;
	}

	public WSAttribute attribute(String name) {
		for (WSAttribute att : getAttributes()) {
			if (att.getName().equals(name))
				return att;
		}
		return null;
	}

	public static WSFolder fromFolder(Folder folder) {
		return fromFolder(folder, true);
	}

	public static WSFolder fromFolder(Folder folder, boolean withCollections) {
		WSFolder wsFolder = new WSFolder();
		wsFolder.setId(folder.getId());
		wsFolder.setFoldRef(folder.getFoldRef());
		wsFolder.setName(folder.getName());
		wsFolder.setType(folder.getType());
		wsFolder.setDescription(folder.getDescription());
		wsFolder.setParentId(folder.getParentId());
		wsFolder.setLastModified(WSUtil.convertDateToString(folder.getLastModified()));
		wsFolder.setCreation(WSUtil.convertDateToString(folder.getCreation()));
		wsFolder.setCreator(folder.getCreator());
		wsFolder.setPosition(folder.getPosition());
		wsFolder.setTemplateLocked(folder.getTemplateLocked());
		wsFolder.setHidden(folder.getHidden());
		wsFolder.setStorage(folder.getStorage());

		if (withCollections && folder.getTags() != null)
			wsFolder.setTags(folder.getTagsAsWords().toArray(new String[0]));

		if (withCollections && folder.getTemplate() != null)
			wsFolder.setTemplateId(folder.getTemplate().getId());

		// Populate the attributes
		if (withCollections) {
			WSAttribute[] attributes = new WSAttribute[0];
			if (folder.getTemplate() != null && folder.getAttributes() != null && folder.getAttributes().size() > 0) {
				attributes = new WSAttribute[folder.getAttributeNames().size()];
				int i = 0;
				for (String name : folder.getAttributeNames()) {
					Attribute attr = folder.getAttribute(name);
					WSAttribute attribute = new WSAttribute();
					attribute.setName(name);
					attribute.setMandatory(attr.getMandatory());
					attribute.setPosition(attr.getPosition());
					attribute.setValue(attr.getValue());

					if (attr.getType() == Attribute.TYPE_USER) {
						attribute.setIntValue(attr.getIntValue());
						attribute.setStringValue(attr.getStringValue());
					}

					attribute.setType(attr.getType());
					attributes[i++] = attribute;
				}
			}
			wsFolder.setAttributes(attributes);
		}

		return wsFolder;
	}

	public void updateAttributes(Folder folder) {
		Template template = null;
		if (templateId != null) {
			folder.getAttributes().clear();
			TemplateDAO templDao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			template = templDao.findById(templateId);
			if (template != null) {
				if (attributes != null && attributes.length > 0) {
					for (int i = 0; i < attributes.length; i++) {
						Attribute extAttribute = new Attribute();
						extAttribute.setMandatory(attributes[i].getMandatory());
						extAttribute.setPosition(attributes[i].getPosition());
						extAttribute.setIntValue(attributes[i].getIntValue());
						extAttribute.setStringValue(attributes[i].getStringValue());
						extAttribute.setDoubleValue(attributes[i].getDoubleValue());
						extAttribute.setDateValue(WSUtil.convertStringToDate(attributes[i].getDateValue()));
						extAttribute.setType(attributes[i].getType());
						folder.getAttributes().put(attributes[i].getName(), extAttribute);
					}
				}
			}
		}

		folder.setTemplate(template);
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public long getParentId() {
		return parentId;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getLastModified() {
		return lastModified;
	}

	public void setLastModified(String lastModified) {
		this.lastModified = lastModified;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public WSAttribute[] getAttributes() {
		return attributes;
	}

	public void setAttributes(WSAttribute[] attributes) {
		this.attributes = attributes;
	}

	public String getCreation() {
		return creation;
	}

	public void setCreation(String creation) {
		this.creation = creation;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public int getTemplateLocked() {
		return templateLocked;
	}

	public void setTemplateLocked(int templateLocked) {
		this.templateLocked = templateLocked;
	}

	public int getHidden() {
		return hidden;
	}

	public void setHidden(int hidden) {
		this.hidden = hidden;
	}

	public Long getFoldRef() {
		return foldRef;
	}

	public void setFoldRef(Long foldRef) {
		this.foldRef = foldRef;
	}

	public Integer getStorage() {
		return storage;
	}

	public void setStorage(Integer storage) {
		this.storage = storage;
	}

	public String[] getTags() {
		return tags;
	}

	public void setTags(String[] tags) {
		this.tags = tags;
	}
}