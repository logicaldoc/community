package com.logicaldoc.core.document;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.security.User;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This class represents versions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 1.0
 */
public class Version extends AbstractDocument implements Comparable<Version> {

	private static final long serialVersionUID = 1L;

	private String username;

	private Date versionDate = new Date();

	private long userId;

	private long folderId;

	private long docId;

	private String folderName;

	private Long templateId;

	private String event;

	private String creator;

	private long creatorId;

	public Version() {
	}

	public Version(Version source) {
		copyAttributes(source);
		setId(source.getId());
		if (source.getIndexed() != INDEX_INDEXED)
			setIndexed(source.getIndexed());
		setCustomId(null);
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
	public boolean equals(Object obj) {
		if (!(obj instanceof Version))
			return false;
		Version other = (Version) obj;
		return other.getId() == this.getId();
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
	public Long getTemplateId() {
		return templateId;
	}

	@Override
	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
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
	public static Version create(Document document, User user, String comment, String event, boolean release) {
		Version version = new Version();
		try {
			BeanUtils.copyProperties(version, document);
		} catch (Exception e) {
			// Nothing to do
		}

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

		version.setPublished(document.getPublished());
		version.setStartPublishing(document.getStartPublishing());
		version.setStopPublishing(document.getStopPublishing());

		String newVersionName = document.getVersion();
		if (!event.equals(DocumentEvent.STORED.toString())) {
			newVersionName = version.getNewVersionName(document.getVersion(), release);
			version.setVersion(newVersionName);
			document.setVersion(newVersionName);
		}

		// If the file changed, than the file version must be changed also
		if (DocumentEvent.CHECKEDIN.toString().equals(event) || DocumentEvent.STORED.toString().equals(event)
				|| StringUtils.isEmpty(document.getFileVersion())) {
			version.setFileVersion(newVersionName);
			document.setFileVersion(newVersionName);
		}

		version.setExtResId(document.getExtResId());
		version.setId(0);
		return version;
	}

	public String getEvent() {
		return event;
	}

	public void setEvent(String event) {
		this.event = event;
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
}