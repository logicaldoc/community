package com.logicaldoc.core.document;

import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.TransactionalObject;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.crypt.CryptUtil;

/**
 * The Document is the central entity of LogicalDOC. A Document is a persistent
 * business object and represents metadata over a single file stored into the
 * DMS.
 * <p>
 * Each document has one or more Versions. The most recent version is the one
 * used as default when we refer to a Document, but all previous versions are
 * accessible from the history even if the are not indexed.
 * <p>
 * Each Version carries out two main informations, the version code itself that
 * is called simply 'version', and the file version, called 'fileVersion'. The
 * first identified the Version itself while the second refers to the file
 * content. In general not all updates to a document involves the upload of a
 * new file.
 * 
 * A Document is written in a single language, this language defines the
 * full-text index in which the document's content will be stored.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public abstract class AbstractDocument extends ExtensibleObject implements TransactionalObject {

	private static final long serialVersionUID = 1L;

	public static final int DOC_UNLOCKED = 0;

	public static final int DOC_CHECKED_OUT = 1;

	public static final int DOC_LOCKED = 2;

	public static final int DOC_ARCHIVED = 3;

	public static final int EXPORT_UNLOCKED = 0;

	public static final int EXPORT_LOCKED = 1;

	public static final int INDEX_TO_INDEX = 0;

	public static final int INDEX_INDEXED = 1;

	public static final int INDEX_SKIP = 2;

	public static final int INDEX_TO_INDEX_METADATA = 3;

	public static final int NATURE_DOC = 0;

	private String comment;

	private long fileSize = 0;

	/**
	 * Whether document is checked out,locked or unlocked
	 * 
	 * @see Document#DOC_UNLOCKED
	 * @see Document#DOC_CHECKED_OUT
	 * @see Document#DOC_LOCKED
	 */
	private int status = DOC_UNLOCKED;

	private int exportStatus = EXPORT_UNLOCKED;

	private String version;

	private String exportVersion;

	private String fileVersion;

	private Date date;

	private String publisher;

	private long publisherId;

	private String creator;

	private long creatorId;

	private String type;

	private Long lockUserId;

	private String lockUser;

	private Date creation = new Date();

	private String language;

	private String fileName;

	/**
	 * The indexing status of the document, one of {@link #INDEX_TO_INDEX},
	 * {@link #INDEX_TO_INDEX_METADATA}, {@link #INDEX_INDEXED} or
	 * {@link #INDEX_SKIP}
	 */
	private int indexed = INDEX_TO_INDEX;

	private int barcoded = 0;

	private int signed = 0;

	private int stamped = 0;

	private Set<Tag> tags = new HashSet<Tag>();

	private String tgs;

	private Folder folder;

	private Template template;

	private String customId;

	private int immutable = 0;

	private String digest;

	private String exportName;

	private Long exportId = null;

	private Long docRef;

	private String docRefType;

	private Long deleteUserId;

	private String deleteUser;

	private Integer rating;

	private String workflowStatus;

	private String workflowStatusDisplay;

	private String color;

	private int published = 1;

	private Date startPublishing = new Date();

	private Date stopPublishing;

	private String transactionId;

	/**
	 * Not persistent. Used sometimes to carry the name of the template
	 */
	private String templateName;

	private int pages = 1;

	private int previewPages = -1;

	private int links = 0;

	/**
	 * Used for saving the external resource ID when editing online
	 */
	private String extResId;

	private int nature = NATURE_DOC;

	private Long formId = null;

	private String password;

	private String decodedPassword;

	/**
	 * Not persistent flag used to indicate that the document was modified and
	 * should be saved back
	 */
	private boolean modified = false;

	/**
	 * Identifier of the Zonal OCR template to use to process this document
	 */
	private Long ocrTemplateId = null;

	/**
	 * Identifier of the barcode template to use to process this document
	 */
	private Long barcodeTemplateId = null;

	/**
	 * Indicates if the document has been processed by the zonal OCR: <b>0</b> =
	 * to process, <b>1</b> = processed
	 */
	private int ocrd = 0;

	public AbstractDocument() {
		super();
	}

	/**
	 * The document status
	 * 
	 * @see Document#DOC_UNLOCKED
	 * @see Document#DOC_CHECKED_OUT
	 * @see Document#DOC_LOCKED
	 * 
	 * @return the document's status
	 */
	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

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
	 * The set of tags for this document.
	 * 
	 * @return the set of tags
	 */
	public Set<Tag> getTags() {
		return tags;
	}

	public void setTags(Set<Tag> tags) {
		this.tags = tags;
	}

	public void setTagsFromWords(Set<String> tgs) {
		if (this.tags != null)
			this.tags.clear();
		else
			this.tags = new HashSet<Tag>();

		if (tgs != null)
			for (String word : tgs) {
				Tag tag = new Tag(getTenantId(), word);
				this.tags.add(tag);
			}
	}

	public Set<String> getTagsAsWords() {
		Set<String> words = new HashSet<String>();
		if (tags != null)
			for (Tag tag : tags) {
				words.add(tag.getTag());
			}
		return words;
	}

	public int getIndexed() {
		return indexed;
	}

	public void setIndexed(int indexed) {
		this.indexed = indexed;
	}

	public String getTagsString() {
		StringBuffer sb = new StringBuffer(",");
		if (tags == null)
			return "";

		Iterator<Tag> iter = tags.iterator();
		boolean start = true;

		while (iter.hasNext()) {
			String words = iter.next().toString();
			words.replace(",", "\\,");

			if (!start) {
				sb.append(",");
			} else {
				start = false;
			}

			sb.append(words);
		}

		sb.append(",");

		return sb.toString();
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
		String icon = IconSelector.selectIcon("", docRef != null && docRef.longValue() != 0L);
		try {
			String extension = fileName.substring(fileName.lastIndexOf(".") + 1);
			icon = IconSelector.selectIcon(extension, docRef != null && docRef.longValue() != 0L);
			if ((getFileName().toLowerCase().endsWith(".eml") || getFileName().toLowerCase().endsWith(".msg"))
					&& getPages() > 1)
				if (docRef != null && docRef.longValue() != 0L)
					icon = "email_attach-sc.png";
				else
					icon = "email_attach.png";
			if (docRef != null && docRef.longValue() != 0L && "pdf".equals(getDocRefType()))
				icon = IconSelector.selectIcon("pdf", true);
		} catch (Exception e) {
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

	/**
	 * Retrieve the folder owning this document
	 * 
	 * @return the parent folder
	 */
	public Folder getFolder() {
		return folder;
	}

	public void setFolder(Folder folder) {
		this.folder = folder;
		if (folder != null)
			this.setTenantId(folder.getTenantId());
	}

	public void addTag(String word) {
		Tag tg = new Tag();
		tg.setTenantId(getTenantId());
		tg.setTag(word);
		tags.add(tg);
	}

	public void clearTags() {
		tags.clear();
		tags = new HashSet<Tag>();
		tgs = null;
	}

	public String getFileExtension() {
		return FilenameUtils.getExtension(getFileName());
	}

	public Template getTemplate() {
		return template;
	}

	public void setTemplate(Template template) {
		this.template = template;
	}

	/**
	 * The document's creation date
	 * 
	 * @return the creation date
	 */
	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
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

	/**
	 * If the document is an alias, it is the id of the referenced document
	 * 
	 * @return identifier of the referenced document
	 */
	public Long getDocRef() {
		return docRef;
	}

	public void setDocRef(Long docRef) {
		this.docRef = docRef;
	}

	public boolean isToIndex() {
		return indexed == INDEX_TO_INDEX;
	}

	public int getBarcoded() {
		return barcoded;
	}

	public void setBarcoded(int barcoded) {
		this.barcoded = barcoded;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
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

	public String getTemplateName() {
		return templateName;
	}

	public void setTemplateName(String templateName) {
		this.templateName = templateName;
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
	 */
	public void setDecodedPassword(String pwd) {
		if (org.apache.commons.lang.StringUtils.isNotEmpty(pwd)) {
			decodedPassword = pwd;
			password = CryptUtil.cryptString(pwd);
		} else {
			decodedPassword = null;
			password = null;
		}
	}

	public String getDecodedPassword() {
		return decodedPassword;
	}

	public String getDeleteUser() {
		return deleteUser;
	}

	public void setDeleteUser(String deleteUser) {
		this.deleteUser = deleteUser;
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
			String test = CryptUtil.cryptString(myPassword);
			return test.equals(getPassword());
		} catch (Throwable t) {
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
		setIndexed(docVO.getIndexed());
		setBarcoded(docVO.getBarcoded());
		setSigned(docVO.getSigned());
		setStamped(docVO.getStamped());
		setDigest(docVO.getDigest());
		setDocRef(docVO.getDocRef());
		setFolder(docVO.getFolder());
		setTemplate(docVO.getTemplate());
		setPages(docVO.getPages());
		setWorkflowStatus(docVO.getWorkflowStatus());
		setWorkflowStatusDisplay(docVO.getWorkflowStatusDisplay());
		setColor(docVO.getColor());

		setAttributes(new HashMap<String, Attribute>());
		try {
			for (String name : docVO.getAttributes().keySet()) {
				getAttributes().put(name, docVO.getAttributes().get(name));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}

		try {
			setTags(new HashSet<Tag>());
			for (Tag tag : docVO.getTags()) {
				getTags().add(tag);
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}
}