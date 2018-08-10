package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;

/**
 * This class represents the key concept of security of documents. The Folder is
 * used as an element to build hierarchies. With foldergroups you can associate
 * groups to a given folder and grant some permissions. Also setting the
 * recurityRef you can specify another reference folder that contains the
 * security policies.
 * <p>
 * Folders have a type: 0 for standard folders, 1 for workspaces.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 6.0
 */
public class Folder extends ExtensibleObject implements Comparable<Folder> {

	public static final long ROOTID = 5;

	public static final long DEFAULTWORKSPACEID = 4;

	public static final String DEFAULTWORKSPACENAME = "Default";

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_WORKSPACE = 1;

	public static final int TYPE_ALIAS = 2;

	private long id = 0;

	private String name = "";

	private long parentId = DEFAULTWORKSPACEID;

	private Long securityRef;

	private String description = "";

	private int type = TYPE_DEFAULT;

	private Date creation = new Date();

	private String creator;

	private Long creatorId;

	private int position = 1;

	private int hidden = 0;

	protected Set<FolderGroup> folderGroups = new HashSet<FolderGroup>();

	/**
	 * Optional default template for this folder
	 */
	private Template template;

	/**
	 * If 1, the users cannot change the template of the contained documents
	 */
	private int templateLocked = 0;

	private Long deleteUserId;

	private Long quotaDocs = null;

	private Long quotaSize = null;

	private Integer quotaThreshold = null;

	private String quotaAlertRecipients = null;
	
	private Long foldRef;

	private Integer storage;

	private Integer maxVersions;

	private String color;

	private Set<Tag> tags = new HashSet<Tag>();

	private String tgs;

	private String path;

	public Folder() {
	}

	public boolean isWorkspace() {
		return type == TYPE_WORKSPACE;
	}
	
	public boolean isAlias() {
		return type == TYPE_ALIAS;
	}
	
	public long getId() {
		return id;
	}

	public long getParentId() {
		return parentId;
	}

	public Set<FolderGroup> getFolderGroups() {
		return folderGroups;
	}

	public void clearFolderGroups() {
		folderGroups.clear();
		folderGroups = new HashSet<FolderGroup>();
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setParentId(long parentId) {
		this.parentId = parentId;
	}

	public void setFolderGroups(Set<FolderGroup> fgroup) {
		folderGroups = fgroup;
	}

	public long[] getFolderGroupIds() {
		long[] idsArray = new long[folderGroups.size()];
		int i = 0;
		for (FolderGroup mg : folderGroups) {
			idsArray[i++] = mg.getGroupId();
		}
		return idsArray;
	}

	/**
	 * Adds FolderGroup object given in a String array to the ArrayList of
	 * FolderGroup.
	 * 
	 * @param groups array of group ids
	 */
	public void setFolderGroup(long[] groups) {
		folderGroups.clear();
		for (int i = 0; i < groups.length; i++) {
			FolderGroup mg = new FolderGroup();
			mg.setGroupId(groups[i]);
			mg.setWrite(1);
			mg.setAdd(1);
			mg.setSecurity(1);
			mg.setDelete(1);
			mg.setRename(1);
			folderGroups.add(mg);
		}
	}

	/**
	 * Adds a new element, substituting a precedin one with the same groupId.
	 */
	public void addFolderGroup(FolderGroup fg) {
		FolderGroup m = getFolderGroup(fg.getGroupId());
		getFolderGroups().remove(m);
		getFolderGroups().add(fg);
	}

	public FolderGroup getFolderGroup(long groupId) {
		for (FolderGroup fg : folderGroups) {
			if (fg.getGroupId() == groupId)
				return fg;
		}
		return null;
	}

	@Override
	public int compareTo(Folder o) {
		int comparation = Integer.compare(this.position, o.position);
		if (comparation != 0)
			return comparation;
		return this.name.compareTo(o.name);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getSecurityRef() {
		return securityRef;
	}

	public void setSecurityRef(Long securityRef) {
		this.securityRef = securityRef;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
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

	public Template getTemplate() {
		return template;
	}

	public void setTemplate(Template template) {
		this.template = template;
	}

	public int getTemplateLocked() {
		return templateLocked;
	}

	public void setTemplateLocked(int templateLocked) {
		this.templateLocked = templateLocked;
	}

	public Long getDeleteUserId() {
		return deleteUserId;
	}

	public void setDeleteUserId(Long deleteUserId) {
		this.deleteUserId = deleteUserId;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public int getHidden() {
		return hidden;
	}

	public void setHidden(int hidden) {
		this.hidden = hidden;
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

	public Set<Tag> getTags() {
		return tags;
	}

	public void setTags(Set<Tag> tags) {
		this.tags = tags;
	}

	public String getTgs() {
		return tgs;
	}

	public void setTgs(String tgs) {
		this.tgs = tgs;
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

	public String getTagsString() {
		StringBuffer sb = new StringBuffer(",");
		if (tags == null)
			return "";

		Iterator<Tag> iter = tags.iterator();
		boolean start = true;

		while (iter.hasNext()) {
			String words = iter.next().toString();

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

	public Integer getQuotaThreshold() {
		return quotaThreshold;
	}

	public void setQuotaThreshold(Integer quotaThreshold) {
		this.quotaThreshold = quotaThreshold;
	}
	

	public String getQuotaAlertRecipients() {
		return quotaAlertRecipients;
	}

	public void setQuotaAlertRecipients(String quotaAlertRecipients) {
		this.quotaAlertRecipients = quotaAlertRecipients;
	}

	public List<String> getQuotaAlertRecipientsAsList() {
		List<String> list = new ArrayList<String>();
		if (!StringUtils.isEmpty(getQuotaAlertRecipients())) {
			StringTokenizer st = new StringTokenizer(getQuotaAlertRecipients(), ",", false);
			while (st.hasMoreTokens())
				list.add(st.nextToken().trim());
		}
		return list;
	}

	public void addQuotaAlertRecipient(String recipient) {
		if (StringUtils.isEmpty(recipient))
			return;
		String str = getQuotaAlertRecipients();
		if (StringUtils.isEmpty(str))
			str = recipient;
		else
			str += "," + recipient;
		setQuotaAlertRecipients(str);
	}
	
	public void setPath(String path) {
		this.path = path;
	}

	public String getPath() {
		return path;
	}
	
	public Folder(String name) {
		this.name = name;
	}
}