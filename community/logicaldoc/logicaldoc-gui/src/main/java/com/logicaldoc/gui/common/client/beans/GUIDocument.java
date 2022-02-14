package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

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

	private long id;

	private Long docRef;

	private String docRefType;

	private String customId;

	private String[] tags = null;

	private String tagsString;

	private String type;

	private String version;

	private String fileVersion;

	private String fileName;

	private Date date;

	private Date creation;

	private String creator;

	private Long creatorId;

	private String publisher;

	private Long publisherId;

	private String language;

	private Float fileSize;

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
	private long[] notifyUsers;

	// Optional message to send to users
	private String notifyMessage;

	private boolean passwordProtected = false;

	private int links = 0;

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
	 * metadata of a bulp update
	 */
	private boolean bulkUpdate = false;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

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

	public String[] getTags() {
		return tags;
	}

	public void setTags(String[] tags) {
		this.tags = tags;
	}

	public void clearTags() {
		this.tags = new String[] {};
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

	public String getTgs() {
		if (getTags() == null || getTags().length < 1)
			return "";
		else {
			StringBuffer buf = new StringBuffer();
			for (String tag : getTags()) {
				if (buf.length() > 0)
					buf.append(",");
				buf.append(tag);
			}
			return buf.toString();
		}
	}

	public String getTagsString() {
		if (tagsString != null && !tagsString.isEmpty())
			return tagsString;
		else {
			StringBuffer buf = new StringBuffer();
			for (String tag : getTags()) {
				buf.append(tag);
				buf.append(" ");
			}
			return buf.toString();
		}
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

	public Float getFileSize() {
		return fileSize;
	}

	public void setFileSize(Float fileSize) {
		this.fileSize = fileSize;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

	@Override
	public boolean equals(Object obj) {
		return id == ((GUIDocument) obj).getId();
	}

	@Override
	public int hashCode() {
		return Long.valueOf(getId()).hashCode();
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

	public long[] getNotifyUsers() {
		return notifyUsers;
	}

	public void setNotifyUsers(long[] notifyUsers) {
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
}