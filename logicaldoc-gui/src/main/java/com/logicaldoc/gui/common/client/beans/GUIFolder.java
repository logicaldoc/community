package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

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

	private GUIAccessControlEntry[] rights = new GUIAccessControlEntry[] {};

	private GUIFolder[] path = null;

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

	private String[] quotaAlertRecipients = new String[0];

	// Total number of documents inside the folder's tree
	private long documentsTotal = 0L;

	// Total size of the folder's tree
	private long sizeTotal = 0L;

	private Long foldRef = null;

	private Integer storage = null;

	private Integer maxVersions = null;

	private String color;

	private String[] tags = null;

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

	public GUIAccessControlEntry[] getRights() {
		return rights;
	}

	public void setRights(GUIAccessControlEntry[] rights) {
		this.rights = rights;
	}

	public long getParentId() {
		return parentId;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public GUIFolder[] getPath() {
		return path;
	}

	public void setPath(GUIFolder[] path) {
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

	public GUIFolder getParent() {
		if (getPath() != null && getPath().length > 0)
			return getPath()[getPath().length - 1];
		else
			return null;
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

	public Integer getStorage() {
		return storage;
	}

	public void setStorage(Integer storage) {
		this.storage = storage;
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

	public String[] getTags() {
		return tags;
	}

	public void setTags(String[] tags) {
		this.tags = tags;
	}

	public void addTag(String tag) {
		String[] tmp = null;
		if (tags != null) {
			tmp = new String[tags.length + 1];

			int i = 0;
			for (String tg : tags) {
				// Skip if the tag already exists
				if (tg.equals(tag))
					return;
				tmp[i++] = tg;
			}
			tmp[i] = tag;
			tags = tmp;
		} else
			tags = new String[] { tag };
	}

	public void removeTag(String tag) {
		if (tags == null || tags.length == 0)
			return;

		String[] tmp = new String[tags.length - 1];
		int i = 0;
		for (String tg : tags) {
			if (!tg.equals(tag) && tmp.length > 0)
				tmp[i++] = tg;
		}
		tags = tmp;
	}

	public String getTagsString() {
		if (tagsString != null && !tagsString.isEmpty())
			return tagsString;
		else {
			StringBuilder buf = new StringBuilder("");
			if (getTags() != null)
				for (String tag : getTags()) {
					buf.append(tag);
					buf.append(" ");
				}
			return buf.toString();
		}
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

	public String[] getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public String getQuotaAlertRecipientsAsString() {
		if (quotaAlertRecipients == null || quotaAlertRecipients.length == 0)
			return null;
		StringBuilder str = new StringBuilder();
		for (String rec : quotaAlertRecipients) {
			if (!str.toString().isEmpty())
				str.append(",");
			str.append(rec.trim());
		}
		return str.toString();
	}

	public void setQuotaAlertRecipients(String[] quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public void clearQuotaAlertRecipients() {
		this.quotaAlertRecipients = new String[] {};
	}

	public void addQuotaAlertRecipient(String recipient) {
		String[] tmp = null;
		if (quotaAlertRecipients != null) {
			tmp = new String[quotaAlertRecipients.length + 1];

			int i = 0;
			for (String tg : quotaAlertRecipients) {
				// Skip if the tag already exists
				if (tg.equals(recipient))
					return;
				tmp[i++] = tg;
			}
			tmp[i] = recipient;
			quotaAlertRecipients = tmp;
		} else
			quotaAlertRecipients = new String[] { recipient };
	}

	public void removeQuotaAlertRecipient(String recipient) {
		if (quotaAlertRecipients == null || quotaAlertRecipients.length == 0)
			return;

		String[] tmp = new String[quotaAlertRecipients.length - 1];
		int i = 0;
		for (String tg : quotaAlertRecipients) {
			if (!tg.equals(recipient) && tmp.length > 0)
				tmp[i++] = tg;
		}
		quotaAlertRecipients = tmp;
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