package com.logicaldoc.webservice.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Document. Useful class to create repository Documents.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@XmlRootElement(name = "document")
@XmlType(name = "WSDocument")
public class WSDocument implements Serializable {

	@WSDoc(documented = false)
	private static final long serialVersionUID = 1L;

	@WSDoc(documented = false)
	protected static Logger log = LoggerFactory.getLogger(WSDocument.class);

	@WSDoc(documented = false)
	public static final int DOC_UNLOCKED = 0;

	@WSDoc(documented = false)
	public static final int DOC_CHECKED_OUT = 1;

	@WSDoc(documented = false)
	public static final int DOC_LOCKED = 2;

	@WSDoc(documented = false)
	public static final int EXPORT_UNLOCKED = 0;

	@WSDoc(documented = false)
	public static final int EXPORT_LOCKED = 1;

	@WSDoc(documented = false)
	public static final int INDEX_TO_INDEX = 0;

	@WSDoc(documented = false)
	public static final int INDEX_INDEXED = 1;

	@WSDoc(documented = false)
	public static final int INDEX_SKIP = 2;

	@WSDoc(description = "unique identifier ")
	private long id;

	private long fileSize = 0;

	/**
	 * Whether document is checked out,locked or unlocked
	 * 
	 * @see Document#DOC_UNLOCKED
	 * @see Document#DOC_CHECKED_OUT
	 * @see Document#DOC_LOCKED
	 */
	@WSDoc(required = false, description = "<b>0</b> = unlocked, <b>1</b> = checked out, <b>2</b> = locked")
	private int status = DOC_UNLOCKED;

	@WSDoc(required = false)
	private int exportStatus = EXPORT_UNLOCKED;

	@WSDoc(required = false)
	private String version;

	@WSDoc(required = false)
	private String fileVersion;

	@WSDoc(description = "last publication date; format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd'")
	private String date;

	@WSDoc(required = false)
	private String publisher;

	@WSDoc(required = false)
	private long publisherId;

	@WSDoc(required = false)
	private String creator;

	@WSDoc(required = false)
	private long creatorId;

	private String type;

	@WSDoc(required = false)
	private Long lockUserId;

	@WSDoc(required = false, description = "the date when the document was created; format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd'")
	private String creation;

	private String fileName;

	@WSDoc(required = false, description = "<b>0</b> = to index, <b>1</b> = indexed, <b>2</b> = skip indexing")
	private int indexed = INDEX_TO_INDEX;

	@WSDoc(required = false)
	private int signed = 0;

	@WSDoc(required = false, description = "<b>1</b> if the document contains at least one stamp")
	private int stamped = 0;

	@WSDoc(required = false, description = "tags applied to the document")
	private String[] tags = new String[0];

	@WSDoc(description = "parent folder")
	private Long folderId;

	@WSDoc(required = false, description = "id of the template assigned to the document")
	private Long templateId;

	@WSDoc(required = false, description = "unique custom identifier")
	private String customId;

	@WSDoc(required = false, description = "<b>0</b> = not immutable, <b>1</b> = immutable")
	private int immutable = 0;

	@WSDoc(required = false)
	private String digest;

	@WSDoc(required = false)
	private String exportName;

	@WSDoc(required = false)
	private Long exportId = null;

	@WSDoc(required = false, description = "used for aliases, refers to another document")
	private Long docRef;

	@WSDoc(required = false, description = "if used, defines the reference type (use pdf for the PDF Conversion)")
	private String docRefType;

	@WSDoc(required = false, description = "user that has deleted the document")
	private Long deleteUserId;

	@WSDoc(required = false, description = "array of attributes")
	private WSAttribute[] attributes = new WSAttribute[0];

	@WSDoc(required = false, description = "language of the document; <a href='/wiki/LanguageSpecification'>See specification</a>")
	private String language;

	@WSDoc(required = false, description = "contains the snippet search text")
	private String summary;

	@WSDoc(required = false, description = "full text search score")
	private Integer score;

	@WSDoc(required = false)
	private String icon;

	@WSDoc(required = false)
	private String comment;

	@WSDoc(required = false)
	private String lastModified;

	@WSDoc(required = false)
	private Integer rating;

	@WSDoc(required = false, description = "Current workflow's status where the document is in")
	private String workflowStatus;

	@WSDoc(required = false, description = "Current workflow's status display informations")
	private String workflowStatusDisplay;

	@WSDoc(required = false, description = "Optional color assigned to the document")
	private String color;
	
	@WSDoc(required = false, description = "If it is not set to <b>1</b>, the document is marked as not published")
	private int published = 1;

	@WSDoc(required = false)
	private String startPublishing;

	@WSDoc(required = false)
	private String stopPublishing;

	@WSDoc(required = false, description = "number of pages (default <b>1</b>)")
	private int pages = -1;

	@WSDoc(required = false, description = "<b>0</b> = document, <b>1</b> = form")
	private int nature = AbstractDocument.NATURE_DOC;

	@WSDoc(required = false, description = "the last modified date; format must be 'yyyy-MM-dd HH:mm:ss' or 'yyyy-MM-dd'")
	private Long formId = null;

	@WSDoc(required = false, description = "indicates whether the document is protected by a password")
	private Integer passwordProtected = 0;

	@WSDoc(required = false, description = "identifier of the Zonal OCR template to use to process this document")
	private Long ocrTemplateId = null;

	@WSDoc(required = false, description = "indicates if the document has been processed by the zonal OCR: <b>0</b> = to process, <b>1</b> = processed")
	private int ocrd = 0;

	@WSDoc(required = false, description = "identifier of the barcode template to use to process this document")
	private Long barcodeTemplateId = null;

	@WSDoc(required = false, description = "indicates if the document has been processed by the barcode processor: <b>0</b> = to process, <b>1</b> = processed")
	private int barcoded = 0;

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

	public long getFileSize() {
		return fileSize;
	}

	public void setFileSize(long fileSize) {
		this.fileSize = fileSize;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public int getExportStatus() {
		return exportStatus;
	}

	public void setExportStatus(int exportStatus) {
		this.exportStatus = exportStatus;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getPublisher() {
		return publisher;
	}

	public void setPublisher(String publisher) {
		this.publisher = publisher;
	}

	public long getPublisherId() {
		return publisherId;
	}

	public void setPublisherId(long publisherId) {
		this.publisherId = publisherId;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(long creatorId) {
		this.creatorId = creatorId;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Long getLockUserId() {
		return lockUserId;
	}

	public void setLockUserId(Long lockUserId) {
		this.lockUserId = lockUserId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
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

	public String[] getTags() {
		return tags;
	}

	public void setTags(String[] tags) {
		this.tags = tags;
	}

	public Long getFolderId() {
		return folderId;
	}

	public void setFolderId(Long folderId) {
		this.folderId = folderId;
	}

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	public String getCustomId() {
		return customId;
	}

	public void setCustomId(String customId) {
		this.customId = customId;
	}

	public int getImmutable() {
		return immutable;
	}

	public void setImmutable(int immutable) {
		this.immutable = immutable;
	}

	public String getDigest() {
		return digest;
	}

	public void setDigest(String digest) {
		this.digest = digest;
	}

	public String getExportName() {
		return exportName;
	}

	public void setExportName(String exportName) {
		this.exportName = exportName;
	}

	public Long getExportId() {
		return exportId;
	}

	public void setExportId(Long exportId) {
		this.exportId = exportId;
	}

	public Long getDocRef() {
		return docRef;
	}

	public void setDocRef(Long docRef) {
		this.docRef = docRef;
	}

	public Long getDeleteUserId() {
		return deleteUserId;
	}

	public void setDeleteUserId(Long deleteUserId) {
		this.deleteUserId = deleteUserId;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getDate() {
		return date;
	}

	public void setDate(String date) {
		this.date = date;
	}

	public String getCreation() {
		return creation;
	}

	public void setCreation(String creation) {
		this.creation = creation;
	}

	public WSAttribute[] getAttributes() {
		return attributes;
	}

	public WSAttribute getAttribute(String name) {
		if (attributes == null)
			return null;
		for (WSAttribute att : attributes) {
			if (att.getName().equals(name))
				return att;
		}
		return null;
	}

	public void setAttributes(WSAttribute[] attributes) {
		this.attributes = attributes;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getSummary() {
		return summary;
	}

	public void setSummary(String summary) {
		this.summary = summary;
	}

	public Integer getScore() {
		return score;
	}

	public void setScore(Integer score) {
		this.score = score;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getLastModified() {
		return lastModified;
	}

	public void setLastModified(String lastModified) {
		this.lastModified = lastModified;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
		this.rating = rating;
	}

	public void addAttribute(WSAttribute att) {
		if (attributes == null)
			attributes = new WSAttribute[0];
		List<WSAttribute> buf = new ArrayList<WSAttribute>();
		for (WSAttribute tmp : attributes)
			buf.add(tmp);
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

	public String getStartPublishing() {
		return startPublishing;
	}

	public void setStartPublishing(String startPublishing) {
		this.startPublishing = startPublishing;
	}

	public String getStopPublishing() {
		return stopPublishing;
	}

	public void setStopPublishing(String stopPublishing) {
		this.stopPublishing = stopPublishing;
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

	public Integer getPasswordProtected() {
		return passwordProtected;
	}

	public void setPasswordProtected(Integer passwordProtected) {
		this.passwordProtected = passwordProtected;
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

	public int getBarcoded() {
		return barcoded;
	}

	public void setBarcoded(int barcoded) {
		this.barcoded = barcoded;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}
}