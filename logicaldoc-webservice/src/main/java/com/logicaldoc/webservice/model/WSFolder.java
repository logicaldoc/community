package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

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
	private static final Logger log = LoggerFactory.getLogger(WSFolder.class);

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

	@WSDoc(description = "another folder to inherit the security from", required = false)
	private Long securityRef;

	@WSDoc(description = "array of attributes", required = false)
	private List<WSAttribute> attributes = new ArrayList<>();

	@WSDoc(description = "the store to use for new files. Valid only in case of workspace.", required = false)
	private Integer store = null;

	@WSDoc(required = false, description = "tags applied to the document")
	private List<String> tags = new ArrayList<>();

	@WSDoc(required = false, description = "identifier of the Zonal OCR template to use to process the documents inside this folder")
	private Long ocrTemplateId = null;

	@WSDoc(required = false, description = "identifier of the barcode template to use to process the documents inside this folder")
	private Long barcodeTemplateId = null;

	@WSDoc(required = false, description = "maximum number of versions maintaned in the workspace")
	private Integer maxVersions;

	@WSDoc(required = false, description = "optional color assigned to the folder")
	private String color;

	@WSDoc(required = false, description = "optional tile image(Base64 encoded) of the folder")
	private String tile;

	public void addAttribute(WSAttribute att) {
		if (attributes == null)
			this.attributes = new ArrayList<>();
		attributes.add(att);
	}

	public void addTag(String tag) {
		if (tags == null)
			tags = new ArrayList<>();
		if (!tags.contains(tag))
			tags.add(tag);
	}

	public Collection<String> listAttributeNames() {
		List<String> names = new ArrayList<>();
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
		wsFolder.setLastModified(DateUtil.format(folder.getLastModified()));
		wsFolder.setCreation(DateUtil.format(folder.getCreation()));
		wsFolder.setCreator(folder.getCreator());
		wsFolder.setPosition(folder.getPosition());
		wsFolder.setTemplateLocked(folder.getTemplateLocked());
		wsFolder.setHidden(folder.getHidden());
		wsFolder.setStore(folder.getStore());
		wsFolder.setMaxVersions(folder.getMaxVersions());
		wsFolder.setSecurityRef(folder.getSecurityRef());
		wsFolder.setOcrTemplateId(folder.getOcrTemplateId());
		wsFolder.setBarcodeTemplateId(folder.getBarcodeTemplateId());
		wsFolder.setColor(folder.getColor());
		wsFolder.setTile(folder.getTile());

		if (withCollections && folder.getTags() != null)
			wsFolder.setTags(new ArrayList<>(folder.getTagsAsWords()));

		if (withCollections && folder.getTemplate() != null)
			wsFolder.setTemplateId(folder.getTemplate().getId());

		// Populate the attributes
		if (withCollections)
			fillAttributes(folder, wsFolder);

		return wsFolder;
	}

	private static void fillAttributes(Folder folder, WSFolder wsFolder) {
		List<WSAttribute> wsAttributes = new ArrayList<>();
		if (folder.getTemplate() != null && folder.getAttributes() != null && folder.getAttributes().size() > 0) {
			for (String name : folder.getAttributeNames()) {
				Attribute attr = folder.getAttribute(name);
				WSAttribute wsAttribute = new WSAttribute();
				wsAttribute.setName(name);
				wsAttribute.setMandatory(attr.getMandatory());
				wsAttribute.setHidden(attr.getHidden());
				wsAttribute.setReadonly(attr.getReadonly());
				wsAttribute.setMultiple(attr.getMultiple());
				wsAttribute.setParent(attr.getParent());
				wsAttribute.setPosition(attr.getPosition());
				wsAttribute.setEditor(attr.getEditor());
				wsAttribute.setValidation(attr.getValidation());
				wsAttribute.setSetId(attr.getSetId());
				wsAttribute.setDateValue(WSUtil.convertDateToString(attr.getDateValue()));
				wsAttribute.setDoubleValue(attr.getDoubleValue());
				wsAttribute.setIntValue(attr.getIntValue());
				wsAttribute.setStringValue(attr.getStringValue());
				wsAttribute.setStringValues(attr.getStringValues());
				wsAttribute.setType(attr.getType());
				wsAttribute.setDependsOn(attr.getDependsOn());				
				wsAttributes.add(wsAttribute);
			}
		}
		wsFolder.setAttributes(wsAttributes);
	}

	public void updateAttributes(Folder folder) throws PersistenceException {
		Template template = null;
		if (templateId != null) {
			folder.getAttributes().clear();
			TemplateDAO templDao = Context.get(TemplateDAO.class);
			template = templDao.findById(templateId);
			if (template != null && CollectionUtils.isNotEmpty(attributes)) {
				for (WSAttribute wsAttribute : attributes) {
					Attribute extAttribute = new Attribute();
					extAttribute.setMandatory(wsAttribute.getMandatory());
					extAttribute.setHidden(wsAttribute.getHidden());
					extAttribute.setReadonly(wsAttribute.getReadonly());
					extAttribute.setMultiple(wsAttribute.getMultiple());
					extAttribute.setParent(wsAttribute.getParent());
					extAttribute.setDependsOn(wsAttribute.getDependsOn());
					extAttribute.setPosition(wsAttribute.getPosition());
					extAttribute.setIntValue(wsAttribute.getIntValue());
					extAttribute.setStringValue(wsAttribute.getStringValue());
					extAttribute.setDoubleValue(wsAttribute.getDoubleValue());
					extAttribute.setDateValue(WSUtil.convertStringToDate(wsAttribute.getDateValue()));
					extAttribute.setType(wsAttribute.getType());
					folder.getAttributes().put(wsAttribute.getName(), extAttribute);
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

	public List<WSAttribute> getAttributes() {
		return attributes;
	}

	public void setAttributes(List<WSAttribute> attributes) {
		if (attributes == null)
			this.attributes = new ArrayList<>();
		else
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

	public Long getSecurityRef() {
		return securityRef;
	}

	public void setSecurityRef(Long securityRef) {
		this.securityRef = securityRef;
	}

	public void setFoldRef(Long foldRef) {
		this.foldRef = foldRef;
	}

	public Integer getStore() {
		return store;
	}

	public void setStore(Integer store) {
		this.store = store;
	}

	public List<String> getTags() {
		return tags;
	}

	public void setTags(List<String> tags) {
		if (tags == null)
			tags = new ArrayList<>();
		this.tags = tags;
	}

	public Long getOcrTemplateId() {
		return ocrTemplateId;
	}

	public void setOcrTemplateId(Long ocrTemplateId) {
		this.ocrTemplateId = ocrTemplateId;
	}

	public Long getBarcodeTemplateId() {
		return barcodeTemplateId;
	}

	public void setBarcodeTemplateId(Long barcodeTemplateId) {
		this.barcodeTemplateId = barcodeTemplateId;
	}

	public Integer getMaxVersions() {
		return maxVersions;
	}

	public void setMaxVersions(Integer maxVersions) {
		this.maxVersions = maxVersions;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public String getTile() {
		return tile;
	}

	public void setTile(String tile) {
		this.tile = tile;
	}
}