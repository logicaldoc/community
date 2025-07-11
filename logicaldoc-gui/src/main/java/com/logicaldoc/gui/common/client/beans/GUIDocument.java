package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Representation of a single document handled by the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIDocument extends GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	public static final int DOC_LOCKED = 2;

	public static final int DOC_UNLOCKED = 0;

	private Long docRef;

	private String docRefType;

	private String customId;

	private List<String> tags = new ArrayList<>();

	private String tagsString;

	private String type;

	private String version;

	private String fileVersion;

	private String revision;

	private String fileName;

	private Date date;

	private Date creation;

	private String creator;

	private Long creatorId;

	private String publisher;

	private Long publisherId;

	private String language;

	private long fileSize = 0;

	private Date lastModified;

	private String pathExtended;

	private GUIFolder folder;

	private String icon = "generic";

	private Long lockUserId;

	private String lockUser;

	private int status = DOC_UNLOCKED;

	private int immutable = 0;

	private int indexed = 0;

	private int signed = 0;

	private int stamped = 0;

	private int rating = 0;

	private boolean bookmarked = false;

	private String comment;

	private String lastNote;

	private String workflowStatus;

	private String workflowStatusDisplay;

	private String color;

	private int published = 1;

	private Date startPublishing = new Date();

	private Date stopPublishing;

	private String summary;

	private int score;

	private String extResId;

	private int pages = 1;

	private int previewPages = 1;

	private int nature = 0;

	private Long formId = null;

	// Users to be notified of the upload
	private List<Long> notifyUsers = new ArrayList<>();

	// Optional message to send to users
	private String notifyMessage;

	private boolean passwordProtected = false;

	private int links = 0;

	/**
	 * Counter of extended attributes of type Document
	 */
	private int docAttrs = 0;

	/**
	 * Identifier of the Zonal OCR template to use to process this document
	 */
	private Long ocrTemplateId = null;

	/**
	 * Indicates if the document has been processed by the zonal OCR: <b>0</b> =
	 * to process, <b>1</b> = processed
	 */
	private int ocrd = 0;

	/**
	 * Identifier of the barcode template to use to process this document
	 */
	private Long barcodeTemplateId = null;

	/**
	 * Indicates if the document has been processed by the barcodes processor:
	 * <b>0</b> = to process, <b>1</b> = processed
	 */
	private int barcoded = 0;

	/**
	 * Just to indicate if this document is being used for collecting the
	 * metadata of a bulk update
	 */
	private boolean bulkUpdate = false;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	/**
	 * Permissions granted to the current user on this document
	 */
	private GUIAccessControlEntry allowedPermissions = new GUIAccessControlEntry();

	public String getCustomId() {
		return customId;
	}

	public void setCustomId(String customId) {
		this.customId = customId;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public List<String> getTags() {
		return tags;
	}

	public void setTags(List<String> tags) {
		this.tags = tags;
	}

	public void clearTags() {
		tags.clear();
	}

	public void addTag(String tag) {
		if (!tags.contains(tag))
			tags.add(tag);
	}

	public void removeTag(String tag) {
		tags.remove(tag);
	}

	public String getTgs() {
		return tags.stream().collect(Collectors.joining(","));
	}

	public String getTagsString() {
		if (tagsString != null && !tagsString.isEmpty())
			return tagsString;
		else
			return tags.stream().collect(Collectors.joining(" "));
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getPublisher() {
		return publisher;
	}

	public void setPublisher(String publisher) {
		this.publisher = publisher;
	}

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public long getFileSize() {
		return fileSize;
	}

	public void setFileSize(long fileSize) {
		this.fileSize = fileSize;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public Long getLockUserId() {
		return lockUserId;
	}

	public void setLockUserId(Long lockUserId) {
		this.lockUserId = lockUserId;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
	}

	public int getImmutable() {
		return immutable;
	}

	public void setImmutable(int immutable) {
		this.immutable = immutable;
	}

	public String getPathExtended() {
		return pathExtended;
	}

	public void setPathExtended(String pathExtended) {
		this.pathExtended = pathExtended;
	}

	public int getRating() {
		return rating;
	}

	public void setRating(int rating) {
		this.rating = rating;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getWorkflowStatus() {
		return workflowStatus;
	}

	public void setWorkflowStatus(String workflowStatus) {
		this.workflowStatus = workflowStatus;
	}

	public String getWorkflowStatusDisplay() {
		return workflowStatusDisplay;
	}

	public void setWorkflowStatusDisplay(String workflowStatusDisplay) {
		this.workflowStatusDisplay = workflowStatusDisplay;
	}

	public int getPublished() {
		return published;
	}

	public void setPublished(int published) {
		this.published = published;
	}

	public Date getStartPublishing() {
		return startPublishing;
	}

	public void setStartPublishing(Date startPublishing) {
		this.startPublishing = startPublishing;
	}

	public Date getStopPublishing() {
		return stopPublishing;
	}

	public void setStopPublishing(Date stopPublishing) {
		this.stopPublishing = stopPublishing;
	}

	public int getBarcoded() {
		return barcoded;
	}

	public void setBarcoded(int barcoded) {
		this.barcoded = barcoded;
	}

	public String getSummary() {
		return summary;
	}

	public void setSummary(String summary) {
		this.summary = summary;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public int getIndexed() {
		return indexed;
	}

	public void setIndexed(int indexed) {
		this.indexed = indexed;
	}

	public int getSigned() {
		return signed;
	}

	public void setSigned(int signed) {
		this.signed = signed;
	}

	public Long getDocRef() {
		return docRef;
	}

	public void setDocRef(Long docRef) {
		this.docRef = docRef;
	}

	public String getExtResId() {
		return extResId;
	}

	public void setExtResId(String extResId) {
		this.extResId = extResId;
	}

	public String getDocRefType() {
		return docRefType;
	}

	public void setDocRefType(String docRefType) {
		this.docRefType = docRefType;
	}

	public int getPages() {
		return pages;
	}

	public void setPages(int pages) {
		this.pages = pages;
	}

	public void setTagsString(String tagsString) {
		this.tagsString = tagsString;
	}

	public int getStamped() {
		return stamped;
	}

	public void setStamped(int stamped) {
		this.stamped = stamped;
	}

	public int getNature() {
		return nature;
	}

	public void setNature(int nature) {
		this.nature = nature;
	}

	public Long getFormId() {
		return formId;
	}

	public void setFormId(Long formId) {
		this.formId = formId;
	}

	public String getLockUser() {
		return lockUser;
	}

	public void setLockUser(String lockUser) {
		this.lockUser = lockUser;
	}

	public List<Long> getNotifyUsers() {
		return notifyUsers;
	}

	public void setNotifyUsers(List<Long> notifyUsers) {
		this.notifyUsers = notifyUsers;
	}

	public String getNotifyMessage() {
		return notifyMessage;
	}

	public void setNotifyMessage(String notifyMessage) {
		this.notifyMessage = notifyMessage;
	}

	public boolean isPasswordProtected() {
		return passwordProtected;
	}

	public void setPasswordProtected(boolean passwordProtected) {
		this.passwordProtected = passwordProtected;
	}

	public boolean isBookmarked() {
		return bookmarked;
	}

	public void setBookmarked(boolean bookmarked) {
		this.bookmarked = bookmarked;
	}

	public int getLinks() {
		return links;
	}

	public void setLinks(int links) {
		this.links = links;
	}

	public Long getOcrTemplateId() {
		return ocrTemplateId;
	}

	public void setOcrTemplateId(Long ocrTemplateId) {
		this.ocrTemplateId = ocrTemplateId;
	}

	public int getOcrd() {
		return ocrd;
	}

	public void setOcrd(int ocrd) {
		this.ocrd = ocrd;
	}

	public Long getBarcodeTemplateId() {
		return barcodeTemplateId;
	}

	public void setBarcodeTemplateId(Long barcodeTemplateId) {
		this.barcodeTemplateId = barcodeTemplateId;
	}

	public Long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(Long creatorId) {
		this.creatorId = creatorId;
	}

	public Long getPublisherId() {
		return publisherId;
	}

	public void setPublisherId(Long publisherId) {
		this.publisherId = publisherId;
	}

	public int getPreviewPages() {
		return previewPages;
	}

	public void setPreviewPages(int previewPages) {
		this.previewPages = previewPages;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public boolean isBulkUpdate() {
		return bulkUpdate;
	}

	public void setBulkUpdate(boolean bulkUpdate) {
		this.bulkUpdate = bulkUpdate;
	}

	public int getDocAttrs() {
		if (docAttrs == 0 && getAttributes().isEmpty())
			for (GUIAttribute att : getAttributes())
				if (att.getType() == GUIAttribute.TYPE_DOCUMENT && att.getIntValue() != null)
					docAttrs++;
		return docAttrs;
	}

	public void setDocAttrs(int docAttrs) {
		this.docAttrs = docAttrs;
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
	
	public boolean isRevision() {
		return allowedPermissions.isRevision();
	}

	public boolean isRead() {
		return allowedPermissions.isRead();
	}

	public boolean isWrite() {
		return allowedPermissions.isWrite();
	}

	public boolean isPreview() {
		return allowedPermissions.isPreview();
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

	public String getLastNote() {
		return lastNote;
	}

	public void setLastNote(String lastNote) {
		this.lastNote = lastNote;
	}

	public String getRevision() {
		return revision;
	}

	public void setRevision(String revision) {
		this.revision = revision;
	}

	public boolean hasPermission(String permission) {
		return allowedPermissions.isPermissionAllowed(permission);
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((fileName == null) ? 0 : fileName.hashCode());
		result = prime * result + ((fileVersion == null) ? 0 : fileVersion.hashCode());
		result = prime * result + ((folder == null) ? 0 : folder.hashCode());
		result = prime * result + ((version == null) ? 0 : version.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		GUIDocument other = (GUIDocument) obj;
		if (fileName == null) {
			if (other.fileName != null)
				return false;
		} else if (!fileName.equals(other.fileName))
			return false;
		if (fileVersion == null) {
			if (other.fileVersion != null)
				return false;
		} else if (!fileVersion.equals(other.fileVersion))
			return false;
		if (folder == null) {
			if (other.folder != null)
				return false;
		} else if (!folder.equals(other.folder))
			return false;
		if (version == null) {
			if (other.version != null)
				return false;
		} else if (!version.equals(other.version))
			return false;
		return true;
	}
}