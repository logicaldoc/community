package com.logicaldoc.core.document;

import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.MappedSuperclass;
import javax.persistence.Transient;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.LazyInitializationException;
import org.hibernate.exception.GenericJDBCException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.TransactionalObject;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.io.FileUtil;

/**
 * The Document is the central entity of LogicalDOC. A Document is a persistent
 * business object and represents metadata over a single file stored into the
 * DMS.
 * <p>
 * Each document has one or more Versions. The most recent version is the one
 * used as default when we refer to a Document, but all previous versions are
 * accessible from the history even if the are not indexed.
 * </p>
 * 
 * <p>
 * Each Version carries out two main informations, the version code itself that
 * is called simply 'version', and the file version, called 'fileVersion'. The
 * first identified the Version itself while the second refers to the file
 * content. In general not all updates to a document involves the upload of a
 * new file.
 * </p>
 * 
 * A Document is written in a single language, this language defines the
 * full-text index in which the document's content will be stored.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
@MappedSuperclass
public abstract class AbstractDocument extends ExtensibleObject implements TransactionalObject {

	private static final Logger log = LoggerFactory.getLogger(AbstractDocument.class);

	private static final long serialVersionUID = 1L;

	/**
	 * Document's export status unlocked: 0
	 */
	public static final int EXPORT_UNLOCKED = 0;

	/**
	 * Document's export status unlocked: 0
	 */
	public static final int EXPORT_LOCKED = 1;

	/**
	 * Document's nature regular document: 0
	 */
	public static final int NATURE_DOC = 0;

	@Column(name = "ld_immutable", nullable = false)
	private int immutable = 0;

	@Column(name = "ld_customid")
	private String customId;

	@Column(name = "ld_comment")
	private String comment;

	/**
	 * The text of the last note put on the document
	 */
	@Column(name = "ld_lastnote")
	private String lastNote;

	@Column(name = "ld_version", length = 10)
	private String version;

	@Column(name = "ld_fileversion", length = 10)
	private String fileVersion;

	@Column(name = "ld_date")
	private Date date;

	@Column(name = "ld_publisher", length = 255)
	private String publisher;

	@Column(name = "ld_publisherid", nullable = false)
	private long publisherId;

	@Column(name = "ld_creator", length = 255)
	private String creator;

	@Column(name = "ld_creatorid", nullable = false)
	private long creatorId;

	@Column(name = "ld_status")
	@Enumerated(EnumType.ORDINAL)
	private DocumentStatus status = DocumentStatus.UNLOCKED;
	
	@Column(name = "ld_indexed", nullable = false)
	@Enumerated(EnumType.ORDINAL)
	private IndexingStatus indexingStatus = IndexingStatus.TO_INDEX;

	@Column(name = "ld_type", length = 255)
	private String type;

	@Column(name = "ld_lockuserid")
	private Long lockUserId;

	@Column(name = "ld_lockuser", length = 255)
	private String lockUser;

	@Column(name = "ld_language", length = 10)
	private String language;

	@Column(name = "ld_filename", length = 255)
	private String fileName;

	@Column(name = "ld_filesize")
	private long fileSize = 0;

	/**
	 * Identifier of the barcode template to use to process this document
	 */
	@Column(name = "ld_barcodetemplateid")
	private Long barcodeTemplateId = null;

	@Column(name = "ld_barcoded", nullable = false)
	private int barcoded = 0;

	@Column(name = "ld_signed", nullable = false)
	private int signed = 0;

	@Column(name = "ld_stamped", nullable = false)
	private int stamped = 0;

	@Column(name = "ld_links", nullable = false)
	private int links = 0;

	/**
	 * Counter of extended attributes of type Document
	 */
	@Column(name = "ld_docattrs", nullable = false)
	private int docAttrs = 0;

	@Column(name = "ld_digest", length = 255)
	private String digest;

	@Column(name = "ld_exportstatus", nullable = false)
	private int exportStatus = EXPORT_UNLOCKED;

	@Column(name = "ld_exportid")
	private Long exportId = null;

	@Column(name = "ld_exportname", length = 255)
	private String exportName;

	@Column(name = "ld_exportversion", length = 10)
	private String exportVersion;

	@Column(name = "ld_deleteuserid")
	private Long deleteUserId;

	@Column(name = "ld_workflowstatus", length = 1000)
	private String workflowStatus;

	@Column(name = "ld_workflowstatusdisp", length = 1000)
	private String workflowStatusDisplay;

	@Column(name = "ld_color", length = 255)
	private String color;

	@Column(name = "ld_published")
	private int published = 1;

	@Column(name = "ld_startpublishing")
	private Date startPublishing = new Date();

	@Column(name = "ld_stoppublishing")
	private Date stopPublishing;

	@Column(name = "ld_transactionid", length = 255)
	private String transactionId;

	/**
	 * Used for saving the external resource ID when editing online
	 */
	@Column(name = "ld_extresid", length = 255)
	private String extResId;

	@Column(name = "ld_tgs", length = 1000)
	protected String tgs;

	@Column(name = "ld_pages", nullable = false)
	private int pages = 1;

	@Column(name = "ld_previewpages", nullable = false)
	private int previewPages = -1;

	@Column(name = "ld_nature", nullable = false)
	private int nature = NATURE_DOC;

	@Column(name = "ld_formid")
	private Long formId = null;

	@Column(name = "ld_password", length = 255)
	private String password;

	/**
	 * Identifier of the Zonal OCR template to use to process this document
	 */
	@Column(name = "ld_ocrtemplateid")
	private Long ocrTemplateId = null;

	/**
	 * Indicates if the document has been processed by the zonal OCR: <b>0</b> =
	 * to process, <b>1</b> = processed
	 */
	@Column(name = "ld_ocrd", nullable = false)
	private int ocrd = 0;

	@Transient
	private String decodedPassword;

	/**
	 * Not persistent flag used to indicate that the document was modified and
	 * should be saved back
	 */
	@Transient
	private boolean modified = false;
	
	@Transient
	private String documentTemplateName;

	@Transient
	private Long documentTemplateId;
	
	protected AbstractDocument() {
		super();
	}

	public abstract long getFolderId();

	/**
	 * The working version (the most recent version)
	 * 
	 * @return the version
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * Iterates over the versions searching for the specified id
	 * 
	 * @param version The version id
	 */
	public void setVersion(String version) {
		this.version = version;
	}
	
	public DocumentStatus getStatus() {
		return status;
	}

	public void setStatus(DocumentStatus status) {
		this.status = status;
	}
	
	public void setStatus(int status) {
		this.status = DocumentStatus.values()[status];
	}
	
	public IndexingStatus getIndexed() {
		return indexingStatus;
	}

	public void setIndexingStatus(IndexingStatus indexingStatus) {
		this.indexingStatus = indexingStatus;
	}
	
	public void setIndexingStatus(int indexingStatus) {
		this.indexingStatus = IndexingStatus.values()[indexingStatus];
	}

	/**
	 * The document's last publication date. This date is altered by checkin
	 * operations.
	 * 
	 * @return the publication date
	 */
	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	/**
	 * The user id of the user that published this document
	 * 
	 * @return identifier of the user that published the file
	 */
	public long getPublisherId() {
		return publisherId;
	}

	public void setPublisherId(long publisherId) {
		this.publisherId = publisherId;
	}

	public boolean isModified() {
		return modified;
	}

	public void setModified(boolean modified) {
		this.modified = modified;
	}

	public Long getDeleteUserId() {
		return deleteUserId;
	}

	public void setDeleteUserId(Long deleteUserId) {
		this.deleteUserId = deleteUserId;
	}

	/**
	 * The username that published this document
	 * 
	 * @return the username that published the document
	 */
	public String getPublisher() {
		return publisher;
	}

	public void setPublisher(String publisher) {
		this.publisher = publisher;
	}

	/**
	 * The document type, that is the file extension
	 * 
	 * @return the right part of the file name
	 */
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	/**
	 * The id of the user that locked this document and that currently locks it
	 * 
	 * @return identifier of the user that owns the lock
	 */
	public Long getLockUserId() {
		return lockUserId;
	}

	public void setLockUserId(Long lockUserId) {
		this.lockUserId = lockUserId;
	}

	/**
	 * The document's language. This attribute is very important because of it
	 * is used to select the right full-text index.
	 * 
	 * @return the language
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * @see Document#setLanguage(java.lang.String)
	 * 
	 * @param language the language
	 */
	public void setLanguage(String language) {
		this.language = language;
	}

	/**
	 * Computes the title that is the file name without the extension
	 * 
	 * @return the left part of the file name
	 */
	public String getTitle() {
		if (fileName != null && fileName.lastIndexOf('.') >= 0)
			return fileName.substring(0, fileName.lastIndexOf('.'));
		else
			return fileName;
	}

	/**
	 * The original file name
	 * 
	 * @return the file name
	 */
	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	/**
	 * The icon for this document, it may be kept from file name extension
	 * 
	 * @return name of the icon file
	 */
	public String getIcon() {
		String icon = IconSelector.selectIcon("");
		try {
			String extension = fileName.substring(fileName.lastIndexOf(".") + 1);
			icon = IconSelector.selectIcon(extension);
			if ((getFileName().toLowerCase().endsWith(".eml") || getFileName().toLowerCase().endsWith(".msg"))
					&& getPages() > 1) {
				icon += "-clip";
			}
		} catch (Exception e) {
			// Nothing to do
		}

		return icon;
	}

	/**
	 * The document's file size expressed in bytes
	 * 
	 * @return the file size in bytes
	 */
	public long getFileSize() {
		return fileSize;
	}

	public void setFileSize(long fileSize) {
		this.fileSize = fileSize;
	}

	public String getFileExtension() {
		return FileUtil.getExtension(getFileName());
	}

	/**
	 * Each document can be identified with a custom identifier
	 * 
	 * @return the unique custom identifier
	 */
	public String getCustomId() {
		return customId;
	}

	public void setCustomId(String customId) {
		this.customId = customId;
	}

	/**
	 * Defines if the document is immutable
	 * 
	 * @return <b>1</b> = immutable, <b>0</b> = regular
	 */
	public int getImmutable() {
		return immutable;
	}

	public void setImmutable(int immutable) {
		this.immutable = immutable;
	}

	/**
	 * The document's digest
	 * 
	 * @return the digest
	 */
	public String getDigest() {
		return digest;
	}

	public void setDigest(String digest) {
		this.digest = digest;
	}

	/**
	 * Return 1 if the document was signed
	 * 
	 * @return <b>1</b> = signed, <b>0</b> = not signed
	 */
	public int getSigned() {
		return signed;
	}

	public void setSigned(int signed) {
		this.signed = signed;
	}

	/**
	 * The working file version. Sometimes the version of the document may
	 * differ from the file versions. In fact if a new version differs from
	 * metadata only, we it have to reference the old file.
	 * 
	 * @return the file version
	 */
	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(long creatorId) {
		this.creatorId = creatorId;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public Locale getLocale() {
		return LocaleUtil.toLocale(getLanguage());
	}

	public void setLocale(Locale locale) {
		setLanguage(locale.toString());
	}

	/**
	 * The document export status
	 * 
	 * @see #EXPORT_UNLOCKED
	 * @see #EXPORT_LOCKED
	 * 
	 * @return the export satus
	 */
	public int getExportStatus() {
		return exportStatus;
	}

	public void setExportStatus(int exportStatus) {
		this.exportStatus = exportStatus;
	}

	/**
	 * The last exported version
	 * 
	 * @return the last exported version
	 */
	public String getExportVersion() {
		return exportVersion;
	}

	public void setExportVersion(String exportVersion) {
		this.exportVersion = exportVersion;
	}

	/**
	 * The last archive name in which the document was exported
	 * 
	 * @return name of the export archive
	 */
	public String getExportName() {
		return exportName;
	}

	public void setExportName(String exportName) {
		this.exportName = exportName;
	}

	/**
	 * The last archive in which the document was exported
	 * 
	 * @return identifier of the export archive
	 */
	public Long getExportId() {
		return exportId;
	}

	public void setExportId(Long exportId) {
		this.exportId = exportId;
	}

	public boolean isToIndex() {
		return indexingStatus == IndexingStatus.TO_INDEX;
	}

	public int getBarcoded() {
		return barcoded;
	}

	public void setBarcoded(int barcoded) {
		this.barcoded = barcoded;
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

	public boolean isPublishing() {
		Date now = new Date();
		if (published != 1)
			return false;
		else if (startPublishing != null && now.before(startPublishing))
			return false;
		else if (stopPublishing == null)
			return true;
		else
			return now.before(stopPublishing);
	}

	@Override
	public String getTransactionId() {
		return transactionId;
	}

	@Override
	public void setTransactionId(String transactionId) {
		this.transactionId = transactionId;
	}

	public String getTgs() {
		return tgs;
	}

	public void setTgs(String tgs) {
		this.tgs = tgs;
	}

	public String getExtResId() {
		return extResId;
	}

	public void setExtResId(String extResId) {
		this.extResId = extResId;
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

	public Long getFormId() {
		return formId;
	}

	public void setFormId(Long formId) {
		this.formId = formId;
	}

	public int getNature() {
		return nature;
	}

	public void setNature(int nature) {
		this.nature = nature;
	}

	public String getLockUser() {
		return lockUser;
	}

	public void setLockUser(String lockUser) {
		this.lockUser = lockUser;
	}

	@Override
	public String toString() {
		return fileName + " (" + super.toString() + ")";
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * Sets the password and encode it
	 * 
	 * @param pwd The password in readable format
	 * 
	 * @throws NoSuchAlgorithmException Cripting error
	 */
	public void setDecodedPassword(String pwd) throws NoSuchAlgorithmException {
		if (org.apache.commons.lang.StringUtils.isNotEmpty(pwd)) {
			decodedPassword = pwd;
			password = CryptUtil.encryptSHA256(pwd);
		} else {
			decodedPassword = null;
			password = null;
		}
	}

	public String getDecodedPassword() {
		return decodedPassword;
	}

	/**
	 * Checks if the document is accessible with the given password
	 * 
	 * @param myPassword the password to check
	 * 
	 * @return true id the password is correct
	 */
	public boolean isGranted(String myPassword) {
		if (StringUtils.isEmpty(getPassword()))
			return true;

		try {
			String test = CryptUtil.encryptSHA256(myPassword);
			return test.equals(getPassword());
		} catch (Exception t) {
			return false;
		}
	}

	public boolean isPasswordProtected() {
		return StringUtils.isNotEmpty(getPassword());
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

	@Override
	public String getTemplateName() {
		return documentTemplateName;
	}

	@Override
	public void setTemplateName(String templateName) {
		this.documentTemplateName = templateName;
	}
	
	
	@Override
	public Long getTemplateId() {
		return documentTemplateId;
	}

	@Override
	public void setTemplateId(Long templateId) {
		this.documentTemplateId = templateId;
	}
	
	/**
	 * Copies in the current instance the attributes of the passed values
	 * object, but NOT the ID
	 * 
	 * @param docVO the document to get the attributes from
	 */
	public void copyAttributes(AbstractDocument docVO) {
		setTenantId(docVO.getTenantId());
		setCustomId(docVO.getCustomId());
		setImmutable(docVO.getImmutable());
		setVersion(docVO.getVersion());
		setFileVersion(docVO.getFileVersion());
		setDate(docVO.getDate());
		setPublisher(docVO.getPublisher());
		setPublisherId(docVO.getPublisherId());
		setCreator(docVO.getCreator());
		setCreatorId(docVO.getCreatorId());
		setStatus(docVO.getStatus());
		setType(docVO.getType());
		setLockUserId(docVO.getLockUserId());
		setLanguage(docVO.getLanguage());
		setFileName(docVO.getFileName());
		setFileSize(docVO.getFileSize());
		setIndexingStatus(docVO.getIndexed());
		setBarcoded(docVO.getBarcoded());
		setSigned(docVO.getSigned());
		setStamped(docVO.getStamped());
		setDigest(docVO.getDigest());
		setTemplate(docVO.getTemplate());
		setPages(docVO.getPages());
		setWorkflowStatus(docVO.getWorkflowStatus());
		setWorkflowStatusDisplay(docVO.getWorkflowStatusDisplay());
		setColor(docVO.getColor());
		setTemplateId(docVO.getTemplateId());
		setTemplateName(docVO.getTemplateName());

		setAttributes(new HashMap<>());
		try {
			for (Entry<String, Attribute> entry : docVO.getAttributes().entrySet())
				getAttributes().put(entry.getKey(), entry.getValue());
		} catch (GenericJDBCException | LazyInitializationException ex) {
			log.debug("Got error when trying to copy collections from document {}", docVO, ex);

			// load again the provided doc
			DocumentDAO docDao = Context.get(DocumentDAO.class);
			try {
				Document testDocVO = docDao.findById(docVO.getId());
				if (testDocVO != null) {
					docDao.initialize(testDocVO);
					for (Entry<String, Attribute> entry : testDocVO.getAttributes().entrySet())
						getAttributes().put(entry.getKey(), entry.getValue());
				}
			} catch (PersistenceException e) {
				log.warn("Cannot copy collections from document {}", docVO, e);
			}
		}
	}

	public void setTagsFromWords(Set<String> tgs) {
		if (CollectionUtils.isEmpty(tgs)) {
			this.tgs = null;
		} else {
			this.tgs = tgs.stream().collect(Collectors.joining(","));
		}
	}

	public int getDocAttrs() {
		return docAttrs;
	}

	public void setDocAttrs(int docAttrs) {
		this.docAttrs = docAttrs;
	}

	public String getLastNote() {
		return lastNote;
	}

	public void setLastNote(String lastNote) {
		this.lastNote = lastNote;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((customId == null) ? 0 : customId.hashCode());
		result = prime * result + ((fileVersion == null) ? 0 : fileVersion.hashCode());
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
		AbstractDocument other = (AbstractDocument) obj;
		if (customId == null) {
			if (other.customId != null)
				return false;
		} else if (!customId.equals(other.customId))
			return false;
		if (fileVersion == null) {
			if (other.fileVersion != null)
				return false;
		} else if (!fileVersion.equals(other.fileVersion))
			return false;
		if (version == null) {
			if (other.version != null)
				return false;
		} else if (!version.equals(other.version))
			return false;
		return true;
	}
}