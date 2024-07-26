package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.Constants;

/**
 * Represents a folder from the GUI view
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIFolder extends GUIExtensibleObject implements Serializable {

	public static final int TYPE_ALIAS = 2;

	public static final int TYPE_WORKSPACE = 1;

	public static final int TYPE_DEFAULT = 0;

	private static final long serialVersionUID = 1L;

	private long id;

	private long parentId;

	private String description;

	private String name;

	/**
	 * Permissions allowed to the current user on this folder
	 */
	private GUIAccessControlEntry allowedPermissions = new GUIAccessControlEntry();

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	private List<GUIFolder> path = new ArrayList<>();

	private GUIFolder securityRef = null;

	private String pathExtended;

	private String creator;

	private Long creatorId;

	private Date creation;

	// Total number of contained documents
	private long documentCount;

	private long subfolderCount;

	private long size;

	private int type;

	private int templateLocked = 0;

	private int position = 1;

	private Long quotaDocs = null;

	private Long quotaSize = null;

	private Integer quotaThreshold = null;

	private List<String> quotaAlertRecipients = new ArrayList<>();

	// Total number of documents inside the folder's tree
	private long documentsTotal = 0L;

	// Total size of the folder's tree
	private long sizeTotal = 0L;

	private Long foldRef = null;

	private Integer store = null;

	private Integer maxVersions = null;

	private String color;

	private List<String> tags = new ArrayList<>();

	private String tagsString;

	private String grid;

	/**
	 * Identifier of the Zonal OCR template to use to process the documents
	 * inside this folder
	 */
	private Long ocrTemplateId = null;

	/**
	 * Identifier of the barcode template to use to process this document
	 */
	private Long barcodeTemplateId = null;

	private String tile;

	public GUIFolder() {

	}

	public boolean isWorkspace() {
		return type == TYPE_WORKSPACE;
	}

	public boolean isDefaultWorkspace() {
		return name.equals(Constants.WORKSPACE_DEFAULTNAME) && type == 1;
	}

	public GUIFolder(long id) {
		this.id = id;
	}

	@Override
	public long getId() {
		return id;
	}

	@Override
	public void setId(long id) {
		this.id = id;
	}

	public GUIAccessControlEntry getAllowedPermissions() {
		return allowedPermissions;
	}

	public void setAllowedPermissions(GUIAccessControlEntry permissions) {
		this.allowedPermissions = permissions;
	}

	public boolean isCustomid() {
		return allowedPermissions.isCustomid();
	}
	
	public boolean isWrite() {
		return allowedPermissions.isWrite();
	}

	public boolean isDownload() {
		return allowedPermissions.isDownload();
	}

	public boolean isMove() {
		return allowedPermissions.isMove();
	}

	public boolean isDelete() {
		return allowedPermissions.isDelete();
	}

	public boolean isRename() {
		return allowedPermissions.isRename();
	}

	public boolean hasPermission(String permission) {
		return allowedPermissions.isPermissionAllowed(permission);
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPathExtended() {
		return pathExtended;
	}

	public void setPathExtended(String pathExtended) {
		this.pathExtended = pathExtended;
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public long getParentId() {
		return parentId;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public List<GUIFolder> getPath() {
		return path;
	}

	public void setPath(List<GUIFolder> path) {
		this.path = path;
		this.pathExtended = "";

		StringBuilder sb = new StringBuilder();
		for (GUIFolder folder : path) {
			if (folder != null && folder.getId() != folder.getParentId()
					&& folder.getId() != Constants.DOCUMENTS_FOLDERID) {
				sb.append("/");
				sb.append(folder.getName());
			}
		}
		sb.append("/");
		sb.append(getName());

		this.pathExtended = sb.toString();
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public Long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(Long creatorId) {
		this.creatorId = creatorId;
	}

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	public long getDocumentCount() {
		return documentCount;
	}

	public void setDocumentCount(long documentCount) {
		this.documentCount = documentCount;
	}

	public long getSubfolderCount() {
		return subfolderCount;
	}

	public void setSubfolderCount(long subfolderCount) {
		this.subfolderCount = subfolderCount;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getTemplateLocked() {
		return templateLocked;
	}

	public void setTemplateLocked(int templateLocked) {
		this.templateLocked = templateLocked;
	}

	public GUIFolder getSecurityRef() {
		return securityRef;
	}

	public void setSecurityRef(GUIFolder securityRef) {
		this.securityRef = securityRef;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public Long getQuotaDocs() {
		return quotaDocs;
	}

	public void setQuotaDocs(Long quotaDocs) {
		this.quotaDocs = quotaDocs;
	}

	public Long getQuotaSize() {
		return quotaSize;
	}

	public void setQuotaSize(Long quotaSize) {
		this.quotaSize = quotaSize;
	}

	public long getDocumentsTotal() {
		return documentsTotal;
	}

	public void setDocumentsTotal(long documentsTotal) {
		this.documentsTotal = documentsTotal;
	}

	public long getSizeTotal() {
		return sizeTotal;
	}

	public void setSizeTotal(long sizeTotal) {
		this.sizeTotal = sizeTotal;
	}

	public Long getFoldRef() {
		return foldRef;
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

	public List<String> getTags() {
		return tags;
	}

	public void setTags(List<String> tags) {
		this.tags = tags;
	}

	public void addTag(String tag) {
		if (!tags.contains(tag))
			tags.add(tag);
	}

	public void removeTag(String tag) {
		tags.remove(tag);
	}

	public String getTagsString() {
		if (tagsString != null && !tagsString.isEmpty())
			return tagsString;
		else
			return tags.stream().collect(Collectors.joining(" "));
	}

	public void setTagsString(String tagsString) {
		this.tagsString = tagsString;
	}

	public Integer getQuotaThreshold() {
		return quotaThreshold;
	}

	public void setQuotaThreshold(Integer quotaThreshold) {
		this.quotaThreshold = quotaThreshold;
	}

	public List<String> getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public String getQuotaAlertRecipientsAsString() {
		return quotaAlertRecipients.stream().collect(Collectors.joining(","));
	}

	public void setQuotaAlertRecipients(List<String> quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public void clearQuotaAlertRecipients() {
		quotaAlertRecipients.clear();
	}

	public void addQuotaAlertRecipient(String recipient) {
		if (!quotaAlertRecipients.contains(recipient))
			quotaAlertRecipients.add(recipient);
	}

	public void removeQuotaAlertRecipient(String recipient) {
		quotaAlertRecipients.remove(recipient);
	}

	public GUIDocument newDocument() {
		GUIDocument document = new GUIDocument();
		document.setBulkUpdate(true);
		document.setStartPublishing(null);
		document.setPublished(-1);

		document.setFolder(this);

		if (getTemplateLocked() == 1) {
			document.setTemplateId(getTemplateId());
			document.setTemplate(getTemplate());
			document.setAttributes(getAttributes());
		}
		document.setOcrTemplateId(getOcrTemplateId());
		document.setBarcodeTemplateId(getBarcodeTemplateId());

		return document;
	}

	@Override
	public String toString() {
		return name;
	}

	public String getGrid() {
		return grid;
	}

	public void setGrid(String grid) {
		this.grid = grid;
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

	public String getTile() {
		return tile;
	}

	public void setTile(String tile) {
		this.tile = tile;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}
}