package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.hibernate.LazyInitializationException;

import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.util.Context;

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

	private static final long serialVersionUID = 1L;

	public static final long ROOTID = 5;

	public static final long DEFAULTWORKSPACEID = 4;

	public static final String DEFAULTWORKSPACENAME = "Default";

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_WORKSPACE = 1;

	public static final int TYPE_ALIAS = 2;

	private String name = "";

	private long parentId = DEFAULTWORKSPACEID;

	private Long securityRef;

	private String description = "";

	private int type = TYPE_DEFAULT;

	private String creator;

	private Long creatorId;

	private int position = 1;

	private int hidden = 0;

	private Set<FolderGroup> folderGroups = new HashSet<>();

	/**
	 * If 1, the users cannot change the template of the contained documents
	 */
	private int templateLocked = 0;

	private Long deleteUserId;

	private String deleteUser;

	private Long quotaDocs = null;

	private Long quotaSize = null;

	private Integer quotaThreshold = null;

	private String quotaAlertRecipients = null;

	private Long foldRef;

	/**
	 * The default storages to use for this folder in the nodes(key: nodeId -
	 * value: storageId)
	 */
	private Map<String, Integer> storages = new HashMap<>();

	private Integer maxVersions;

	private String color;

	private Set<Tag> tags = new HashSet<>();

	/**
	 * Comma-separated tags, used for searching only
	 */
	private String tgs;

	/**
	 * Stores the absolute path of the folder composed by IDs like /5/4/12354
	 */
	private String path;

	/**
	 * Transient attribute that stores a human readable path like
	 * /Default/invoices
	 */
	private String pathExtended;

	/**
	 * Description of the grid that displays the list of documents
	 */
	private String grid;

	/**
	 * Identifier of the Zonal OCR template to use to process the documents
	 * inside this folder
	 */
	private Long ocrTemplateId = null;

	/**
	 * Identifier of the barcode template to use to process the documents inside
	 * this folder
	 */
	private Long barcodeTemplateId = null;

	private String tile;

	public Folder() {
	}

	public Folder(Folder source) {
		this.setId(source.getId());
		this.name = source.name;
		this.parentId = source.parentId;
		this.securityRef = source.securityRef;
		this.description = source.description;
		this.type = source.type;
		this.setCreation(source.getCreation());
		this.creator = source.creator;
		this.creatorId = source.creatorId;
		this.position = source.position;
		this.hidden = source.hidden;
		this.folderGroups = source.folderGroups;
		this.templateLocked = source.templateLocked;
		this.deleteUserId = source.deleteUserId;
		this.deleteUser = source.deleteUser;
		this.quotaDocs = source.quotaDocs;
		this.quotaSize = source.quotaSize;
		this.quotaThreshold = source.quotaThreshold;
		this.quotaAlertRecipients = source.quotaAlertRecipients;
		this.foldRef = source.foldRef;
		this.storages = source.storages;
		this.maxVersions = source.maxVersions;
		this.color = source.color;
		this.tags = source.tags;
		this.tgs = source.tgs;
		this.path = source.path;
		this.pathExtended = source.pathExtended;
		this.grid = source.grid;
		this.ocrTemplateId = source.ocrTemplateId;
		this.barcodeTemplateId = source.barcodeTemplateId;
		this.tile = source.tile;

		setTemplate(source.getTemplate());
		setTemplateId(source.getTemplateId());
		setTemplateName(source.getTemplateName());

		setTenantId(source.getTenantId());

		setAttributes(new HashMap<>());
		try {
			for (String attName : source.getAttributes().keySet()) {
				getAttributes().put(attName, source.getAttributes().get(attName));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}

		try {
			setTags(new HashSet<>());
			for (Tag tag : source.getTags()) {
				getTags().add(tag);
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
		try {
			setFolderGroups(new HashSet<>());
			for (FolderGroup fg : source.getFolderGroups()) {
				getFolderGroups().add(new FolderGroup(fg));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}

		setStorages(new HashMap<>());
		try {
			for (String nodeId : source.getStorages().keySet()) {
				getStorages().put(nodeId, source.getStorages().get(nodeId));
			}
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	public boolean isWorkspace() {
		return type == TYPE_WORKSPACE;
	}

	public boolean isAlias() {
		return type == TYPE_ALIAS;
	}

	public long getParentId() {
		return parentId;
	}

	public Set<FolderGroup> getFolderGroups() {
		return folderGroups;
	}

	public void clearFolderGroups() {
		folderGroups.clear();
		folderGroups = new HashSet<>();
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
	 * Adds a new element, substituting an existing one with the same groupId.
	 * 
	 * @param fg the folder group
	 */
	public void addFolderGroup(FolderGroup fg) {
		FolderGroup m = getFolderGroup(fg.getGroupId());
		if (m != null)
			getFolderGroups().remove(m);
		if (fg.getRead() != 0)
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
		int comparison = Integer.compare(this.position, o.position);
		if (comparison != 0)
			return comparison;
		return this.name.compareTo(o.name);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Folder))
			return false;
		Folder other = (Folder) obj;
		return other.getId() == this.getId();
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

	/**
	 * Gets the default storage to use from this folder in the current node
	 * 
	 * @return identifier of the default storage
	 */
	public Integer getStorage() {
		try {
			return storages.get(Context.get().getProperties().get("id"));
		} catch (Exception t) {
			return null;
		}
	}

	/**
	 * Gets the default storage to use from this folder in the current node
	 * 
	 * @param storage identifier of the default storage
	 */
	public void setStorage(Integer storage) {
		try {
			String nodeId = Context.get().getProperties().getProperty("id");
			if (storage == null)
				storages.remove(nodeId);
			else
				storages.put(nodeId, storage);
		} catch (Exception t) {
			// Nothing to do
		}
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

	/**
	 * So not invoke this method directly, it is thought to be used by the ORM
	 * inside the DAO.
	 * 
	 * @param tgs comma-separated string of tags
	 */
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
		tags = new HashSet<>();
		tgs = null;
	}

	public void setTagsFromWords(Set<String> tgs) {
		if (this.tags != null)
			this.tags.clear();
		else
			this.tags = new HashSet<>();

		if (tgs != null)
			for (String word : tgs) {
				Tag tag = new Tag(getTenantId(), word);
				this.tags.add(tag);
			}
	}

	public Set<String> getTagsAsWords() {
		Set<String> words = new HashSet<>();
		if (tags != null)
			for (Tag tag : tags) {
				words.add(tag.getTag());
			}
		return words;
	}

	public String getGrid() {
		return grid;
	}

	public void setGrid(String grid) {
		this.grid = grid;
	}

	public String getTagsString() {
		StringBuilder sb = new StringBuilder(",");
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
		List<String> list = new ArrayList<>();
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

	public String getDeleteUser() {
		return deleteUser;
	}

	public void setDeleteUser(String deleteUser) {
		this.deleteUser = deleteUser;
	}

	public String getPathExtended() {
		return pathExtended;
	}

	public void setPathExtended(String pathExtended) {
		this.pathExtended = pathExtended;
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

	@Override
	public String toString() {
		return getName() + "(" + getId() + ")";
	}

	public Map<String, Integer> getStorages() {
		return storages;
	}

	public void setStorages(Map<String, Integer> storages) {
		this.storages = storages;
	}

	public String getTile() {
		return tile;
	}

	public void setTile(String tile) {
		this.tile = tile;
	}
}