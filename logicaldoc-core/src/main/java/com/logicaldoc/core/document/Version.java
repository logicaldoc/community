package com.logicaldoc.core.document;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.config.ContextProperties;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapKeyColumn;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;

/**
 * This class represents versions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 1.0
 */
@Entity
@Table(name = "ld_version")
@Cacheable
public class Version extends AbstractDocument implements Comparable<Version> {

	private static final long serialVersionUID = 1L;

	@Column(name = "ld_documentid", nullable = false)
	private long docId;

	@Column(name = "ld_username", length = 255)
	private String username;

	@Column(name = "ld_userid")
	private long userId;

	@Column(name = "ld_versiondate", columnDefinition = "DATETIME(3)")
	private Date versionDate = new Date();

	@Column(name = "ld_folderid")
	private long folderId;

	@Column(name = "ld_foldername", length = 1000)
	private String folderName;

	@Column(name = "ld_event", length = 255)
	private String event;

	@Column(name = "ld_creator", length = 255)
	private String creator;

	@Column(name = "ld_creatorid", nullable = false)
	private long creatorId;

	@Column(name = "ld_templateid")
	private Long templateId;

	@Column(name = "ld_templatename")
	private String templateName;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "ld_version_ext", joinColumns = @JoinColumn(name = "ld_versionid"))
	@MapKeyColumn(name = "ld_name", length = 255)
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, Attribute> attributes = new HashMap<>();

	public Version() {
	}

	public Version(Version source) {
		copyAttributes(source);
		setId(source.getId());
		setFolderId(source.getFolderId());
		if (source.getIndexed() != IndexingStatus.INDEXED)
			setIndexingStatus(source.getIndexed());
		setCustomId(null);
	}

	@Override
	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	@Override
	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * Calculate the new version name in the format <b>X</b>.<b>Y</b>.
	 * 
	 * <ul>
	 * <li>if the new version is a release, then X will be raised by 1 and Y
	 * will be 0 (e.g.: 12.3 will become 13.0)</li>
	 * <li>if the new version is not a release, then Y will be raised by 1
	 * (e.g.: 12.3 will become 12.4)</li>
	 * </ul>
	 * 
	 * @param oldVersionName the old version in the format <b>X</b>.<b>Y</b>
	 * @param major if the new version is a major release or not
	 * 
	 * @return the new version name in the format <b>X</b>.<b>Y</b>
	 */
	private String getNewVersionName(String oldVersionName, boolean major) {
		if (StringUtils.isEmpty(oldVersionName)) {
			ContextProperties config;
			try {
				config = new ContextProperties();
				return config.getProperty("document.startversion");
			} catch (IOException e) {
				return "1.0";
			}
		}

		return calculateNewVersion(oldVersionName, major);
	}

	/**
	 * Calculate the new version name in the format <b>X</b>.<b>Y</b>.
	 * 
	 * @see #getNewVersionName(String, boolean)
	 * 
	 * @param oldVersionName the old version in the format <b>X</b>.<b>Y</b>
	 * @param major if the new version is a major release or not
	 * 
	 * @return the new version name in the format <b>X</b>.<b>Y</b>
	 */
	public static String calculateNewVersion(String oldVersionName, boolean major) {
		String rel = oldVersionName.substring(0, oldVersionName.indexOf("."));
		String version = oldVersionName.substring(oldVersionName.lastIndexOf(".") + 1);

		int number;
		if (major) {
			number = Integer.parseInt(rel);
			rel = String.valueOf(number + 1);
			version = "0";
		} else {
			number = Integer.parseInt(version);
			version = String.valueOf(number + 1);
		}

		return rel + "." + version;
	}

	public int compareTo(Version other) {
		try {
			StringTokenizer st1 = new StringTokenizer(getVersion().trim(), ".", false);
			StringTokenizer st2 = new StringTokenizer(other.getVersion().trim(), ".", false);

			Integer num1 = Integer.parseInt(st1.nextToken());
			Integer num2 = Integer.parseInt(st2.nextToken());

			if (num1.compareTo(num2) != 0)
				return num1.compareTo(num2);

			num1 = Integer.parseInt(st1.nextToken());
			num2 = Integer.parseInt(st2.nextToken());

			return num1.compareTo(num2);
		} catch (Exception t) {
			if (this.getDate() != null && other.getDate() != null)
				return this.getDate().compareTo(other.getDate());
			else
				return -1;
		}
	}

	@Override
	public String toString() {
		return getVersion() + (getComment() != null ? "-" + getComment() : "");
	}

	public Date getVersionDate() {
		return versionDate;
	}

	public void setVersionDate(Date versionDate) {
		this.versionDate = versionDate;
	}

	public long getFolderId() {
		return folderId;
	}

	public void setFolderId(long folderId) {
		this.folderId = folderId;
	}

	public String getFolderName() {
		return folderName;
	}

	public void setFolderName(String folderName) {
		this.folderName = folderName;
	}

	@Override
	public String getTemplateName() {
		return templateName;
	}

	@Override
	public void setTemplateName(String templateName) {
		this.templateName = templateName;
	}

	/**
	 * Factory method that creates a Version and replicate all given document's
	 * properties.<br>
	 * The new version and fileVersion will be set in both Document and
	 * Version<br>
	 * <br>
	 * <b>Important:</b> The created Version is not persistent
	 * 
	 * @param document The document to be versioned
	 * @param user The user who made the changes
	 * @param comment The version comment
	 * @param event The event that caused the new release
	 * @param release True if this is a new release(eg: 2.0) rather than a
	 *        subversion(eg: 1.1)
	 * @return The newly created version
	 */
	public static Version create(Document document, User user, String comment, DocumentEvent event, boolean release) {
		Version version = new Version();
		try {
			BeanUtils.copyProperties(version, document);
		} catch (Exception e) {
			// Nothing to do
		}

		version.setRevision(document.getRevision() != null ? document.getRevision() : document.getVersion());
		version.setVersion(document.getVersion());
		version.setType(document.getType());
		version.setTenantId(document.getTenantId());
		version.setDeleted(0);
		version.setRecordVersion(0);
		version.setComment(comment);
		document.setComment(comment);
		version.setEvent(event);
		version.setUserId(user.getId());
		version.setUsername(user.getFullName());
		version.setOcrTemplateId(document.getOcrTemplateId());
		version.setBarcodeTemplateId(document.getBarcodeTemplateId());
		version.setWorkflowStatus(document.getWorkflowStatus());
		version.setWorkflowStatusDisplay(document.getWorkflowStatusDisplay());
		version.setColor(document.getColor());
		version.setLastNote(document.getLastNote());

		if (document.getTemplate() != null) {
			version.setTemplateId(document.getTemplate().getId());
			version.setTemplateName(document.getTemplate().getName());
		}

		version.setAttributes(new HashMap<>());
		if (document.getAttributes() != null) {
			try {
				for (String name : document.getAttributeNames()) {
					version.getAttributes().put(name, document.getAttributes().get(name));
				}
			} catch (Exception t) {
				// Nothing to do
			}
		}

		version.setFolderId(document.getFolder().getId());
		version.setFolderName(document.getFolder().getName());
		version.setTgs(document.getTagsString());
		version.setDocId(document.getId());
		version.setLinks(document.getLinks());
		version.setDocAttrs(document.getDocAttrs());

		version.setPublished(document.isPublished());
		version.setStartPublishing(document.getStartPublishing());
		version.setStopPublishing(document.getStopPublishing());

		String newVersionName = document.getVersion();
		if (!event.equals(DocumentEvent.STORED)) {
			newVersionName = version.getNewVersionName(document.getVersion(), release);
			version.setVersion(newVersionName);
			document.setVersion(newVersionName);
		}

		// If the file changed, than the file version must be changed also
		if (DocumentEvent.CHECKEDIN.equals(event) || DocumentEvent.STORED.equals(event)
				|| StringUtils.isEmpty(document.getFileVersion())) {
			version.setFileVersion(newVersionName);
			document.setFileVersion(newVersionName);
		}

		version.setExtResId(document.getExtResId());
		version.setId(0);
		return version;
	}

	public void setEvent(DocumentEvent event) {
		this.event = (event != null) ? event.toString() : null;
	}

	public DocumentEvent getEventEnum() {
		if (event == null)
			return null;
		return DocumentEvent.fromKey(event);
	}

	public String getEvent() {
		return event;
	}

	@Override
	public String getCreator() {
		return creator;
	}

	@Override
	public void setCreator(String creator) {
		this.creator = creator;
	}

	@Override
	public long getCreatorId() {
		return creatorId;
	}

	@Override
	public void setCreatorId(long creatorId) {
		this.creatorId = creatorId;
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	@Override
	public Long getTemplateId() {
		return templateId;
	}

	@Override
	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	@Override
	public Template getTemplate() {
		return null;
	}

	@Override
	public void setTemplate(Template template) {
		if (template != null) {
			templateId = template.getId();
			setTemplateName(template.getName());
		} else {
			templateId = null;
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (creatorId ^ (creatorId >>> 32));
		result = prime * result + (int) (docId ^ (docId >>> 32));
		result = prime * result + ((versionDate == null) ? 0 : versionDate.hashCode());
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
		Version other = (Version) obj;
		if (creatorId != other.creatorId)
			return false;
		if (docId != other.docId)
			return false;
		if (versionDate == null) {
			if (other.versionDate != null)
				return false;
		} else if (!versionDate.equals(other.versionDate))
			return false;
		return true;
	}
}