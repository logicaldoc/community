package com.logicaldoc.core.document;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang.StringUtils;

import com.ibm.icu.util.StringTokenizer;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.User;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This class represents versions.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 1.0
 */
public class Version extends AbstractDocument implements Comparable<Version> {

	public final static String EVENT_STORED = "event.stored";

	public final static String EVENT_CHANGED = "event.changed";

	public final static String EVENT_CHECKIN = "event.checkedin";

	public static final String EVENT_RENAMED = "event.renamed";

	public final static String EVENT_MOVED = "event.moved";

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

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	/**
	 * @see Version#getUsername()
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * @see Version#setUsername(java.lang.String)
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @see Version#getNewVersionName(java.lang.String,
	 *      VersionImpl.VERSION_TYPE)
	 */
	private String getNewVersionName(String oldVersionName, boolean release) {
		if (StringUtils.isEmpty(oldVersionName)) {
			ContextProperties config;
			try {
				config = new ContextProperties();
				return config.getProperty("document.startversion");
			} catch (IOException e) {
				return "1.0";
			}
		}

		String rel = oldVersionName.substring(0, oldVersionName.indexOf("."));
		String version = oldVersionName.substring(oldVersionName.lastIndexOf(".") + 1);

		int number;
		if (release) {
			number = Integer.parseInt(rel);
			rel = String.valueOf(number + 1);
			version = "0";
		} else {
			number = Integer.parseInt(version);
			version = String.valueOf(number + 1);
		}

		return rel + "." + version;
	}

	/** For sorting a list of Version objects by the version number */
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
		} catch (Throwable t) {
			if (this.getDate() != null && other.getDate() != null)
				return this.getDate().compareTo(other.getDate());
			else
				return -1;
		}
	}

	@Override
	public String toString() {
		return getVersion() + getComment() != null ? "-" + getComment() : "";
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

	public Long getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Long templateId) {
		this.templateId = templateId;
	}

	/**
	 * Factory method that creates a Version and replicate all given document's
	 * properties.<br />
	 * The new version and fileVersion will be set in both Document and Version<br/>
	 * <br/>
	 * <b>Important:</b> The created Version is not persistent
	 * 
	 * @param document The document to be versioned
	 * @param user The user who made the changes
	 * @param comment The version comment
	 * @param event The event that caused the new release
	 * @param release True if this is a new release(eg: 2.0) rather than a
	 *        subversion(eg: 1.1)
	 * @param initial True if this is an initial release
	 * @return The newly created version
	 */
	public static Version create(Document document, User user, String comment, String event, boolean release) {
		Version version = new Version();
		try {
			BeanUtils.copyProperties(version, document);
		} catch (Exception e) {
			
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

		if (document.getTemplate() != null) {
			version.setTemplateId(document.getTemplate().getId());
			version.setTemplateName(document.getTemplate().getName());
		}

		version.setAttributes(new HashMap<String, Attribute>());
		if (document.getAttributes() != null) {
			try {
				for (String name : document.getAttributeNames()) {
					version.getAttributes().put(name, document.getAttributes().get(name));
				}
			} catch (Throwable t) {
			}
		}

		version.setFolderId(document.getFolder().getId());
		version.setFolderName(document.getFolder().getName());
		version.setTgs(document.getTagsString());
		version.setDocId(document.getId());

		version.setPublished(document.getPublished());
		version.setStartPublishing(document.getStartPublishing());
		version.setStopPublishing(document.getStopPublishing());

		String newVersionName = document.getVersion();
		if (!event.equals(Version.EVENT_STORED)) {
			newVersionName = version.getNewVersionName(document.getVersion(), release);
			version.setVersion(newVersionName);
			document.setVersion(newVersionName);
		}

		// If the file changed, than the file version must be changed also
		if (Version.EVENT_CHECKIN.equals(event) || Version.EVENT_STORED.equals(event)
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

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}
}