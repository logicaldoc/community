package com.logicaldoc.core.folder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.document.FolderAccessControlEntry;
import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.Secure;
import com.logicaldoc.util.spring.Context;

import jakarta.persistence.Cacheable;
import jakarta.persistence.CascadeType;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.MapKeyColumn;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

/**
 * This class represents the key concept of security of documents. The Folder is
 * used as an element to build hierarchies. With the AccessControlList you can
 * associate groups to a given folder and grant some permissions. Also setting
 * the recurityRef you can specify another reference folder that contains the
 * security policies.
 * <p>
 * Folders have a type: 0 for standard folders, 1 for workspaces.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 6.0
 */
@Entity
@Table(name = "ld_folder")
@Cacheable
public class Folder extends ExtensibleObject implements Secure<FolderAccessControlEntry>, Comparable<Folder> {

	private static final long serialVersionUID = 1L;

	public static final long ROOTID = 5;

	public static final long DEFAULTWORKSPACEID = 4;

	public static final String DEFAULTWORKSPACENAME = "Default";

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_WORKSPACE = 1;

	public static final int TYPE_ALIAS = 2;

	@Column(name = "ld_name", length = 255)
	private String name = "";

	@Column(name = "ld_position", nullable = false)
	private int position = 1;

	@Column(name = "ld_parentid", nullable = false)
	private long parentId = DEFAULTWORKSPACEID;

	@Column(name = "ld_foldref")
	private Long foldRef;

	@Column(name = "ld_securityref")
	private Long securityRef;

	@Column(name = "ld_description", length = 4000)
	private String description = "";

	@Column(name = "ld_type", nullable = false)
	private int type = TYPE_DEFAULT;

	@Column(name = "ld_creator", length = 255)
	private String creator;

	@Column(name = "ld_creatorid")
	private Long creatorId;

	/**
	 * If true, the users cannot change the template of the contained documents
	 */
	@Column(name = "ld_templocked", nullable = false)
	private boolean templateLocked = false;

	@Column(name = "ld_deleteuserid")
	private Long deleteUserId;

	@Column(name = "ld_deleteuser", length = 255)
	private String deleteUser;

	@Column(name = "ld_hidden", nullable = false)
	private boolean hidden = false;

	@Column(name = "ld_quotadocs")
	private Long quotaDocs = null;

	@Column(name = "ld_quotasize")
	private Long quotaSize = null;

	@Column(name = "ld_qthreshold")
	private Integer quotaThreshold = null;

	@Column(name = "ld_qrecipients", length = 1000)
	private String quotaAlertRecipients = null;

	@Column(name = "ld_maxversions")
	private Integer maxVersions;

	@Column(name = "ld_color", length = 255)
	private String color;

	/**
	 * Stores the absolute path of the folder composed by IDs like /5/4/12354
	 */
	@Column(name = "ld_path")
	private String path;

	/**
	 * Description of the grid that displays the list of documents
	 */
	@Column(name = "ld_grid")
	private String grid;

	/**
	 * Identifier of the Zonal OCR template to use to process the documents
	 * inside this folder
	 */
	@Column(name = "ld_ocrtemplateid")
	private Long ocrTemplateId = null;

	/**
	 * Identifier of the barcode template to use to process the documents inside
	 * this folder
	 */
	@Column(name = "ld_barcodetemplateid")
	private Long barcodeTemplateId = null;

	@Column(name = "ld_tile")
	private String tile;

	/**
	 * Comma-separated tags, used for searching only
	 */
	@Column(name = "ld_tgs", length = 1000)
	private String tgs;

	/**
	 * The default stores to use for this folder in the nodes(key: nodeId -
	 * value: storeId)
	 */
	@ElementCollection
	@CollectionTable(name = "ld_folder_store", joinColumns = @JoinColumn(name = "ld_folderid"))
	@MapKeyColumn(name = "ld_nodeid")
	@Column(name = "ld_storeid")
	private Map<String, Integer> stores = new HashMap<>();

	@ElementCollection
	@CollectionTable(name = "ld_foldertag", joinColumns = @JoinColumn(name = "ld_folderid"))
	@OrderBy("ld_tag")
	private Set<Tag> tags = new HashSet<>();

	/**
	 * Transient attribute that stores a human readable path like
	 * /Default/invoices
	 */
	@Transient
	private String pathExtended;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "ld_folder_acl", joinColumns = @JoinColumn(name = "ld_folderid"))
	private Set<FolderAccessControlEntry> accessControlList = new HashSet<>();

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(name = "ld_folder_ext", joinColumns = @JoinColumn(name = "ld_folderid"))
	@MapKeyColumn(name = "ld_name")
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, Attribute> attributes = new HashMap<>();

	@ManyToOne(cascade = CascadeType.DETACH)
	@JoinColumn(name = "ld_templateid")
	private Template template;

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
		this.templateLocked = source.templateLocked;
		this.deleteUserId = source.deleteUserId;
		this.deleteUser = source.deleteUser;
		this.quotaDocs = source.quotaDocs;
		this.quotaSize = source.quotaSize;
		this.quotaThreshold = source.quotaThreshold;
		this.quotaAlertRecipients = source.quotaAlertRecipients;
		this.foldRef = source.foldRef;
		this.stores = source.stores;
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

		try {
			if (source instanceof Folder folder)
				for (FolderAccessControlEntry ace : folder.getAccessControlList())
					getAccessControlList().add(new FolderAccessControlEntry(ace));
		} catch (org.hibernate.LazyInitializationException x) {
			// may happen do nothing
		}

		setAttributes(new HashMap<>());
		try {
			for (String attName : source.getAttributes().keySet()) {
				getAttributes().put(attName, source.getAttributes().get(attName));
			}
		} catch (org.hibernate.LazyInitializationException x) {
			// may happen do nothing
		}

		try {
			setTags(new HashSet<>());
			for (Tag tag : source.getTags()) {
				getTags().add(tag);
			}
		} catch (org.hibernate.LazyInitializationException x) {
			// may happen do nothing
		}

		setStores(new HashMap<>());
		try {
			for (String nodeId : source.getStores().keySet()) {
				getStores().put(nodeId, source.getStores().get(nodeId));
			}
		} catch (org.hibernate.LazyInitializationException x) {
			// may happen do nothing
		}
	}

	@Override
	public Set<FolderAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	@Override
	public void setAccessControlList(Set<FolderAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}
	
	@Override
	public void removeAccessControlEntries(long groupId) {
		getAccessControlList().removeIf(ace -> ace.getGroupId() == groupId);
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

	public void setParentId(long parentId) {
		this.parentId = parentId;
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

	public boolean isTemplateLocked() {
		return templateLocked;
	}

	public void setTemplateLocked(boolean templateLocked) {
		this.templateLocked = templateLocked;
	}

	public boolean isHidden() {
		return hidden;
	}

	public void setHidden(boolean hidden) {
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
	 * Gets the default store to use from this folder in the current node
	 * 
	 * @return identifier of the default store
	 */
	public Integer getStore() {
		try {
			return stores.get(Context.get().getConfig().get("id"));
		} catch (Exception t) {
			return null;
		}
	}

	/**
	 * Gets the default store to use from this folder in the current node
	 * 
	 * @param store identifier of the default store
	 */
	public void setStore(Integer store) {
		try {
			String nodeId = Context.get().getConfig().getProperty("id");
			if (store == null)
				stores.remove(nodeId);
			else
				stores.put(nodeId, store);
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

	public Map<String, Integer> getStores() {
		return stores;
	}

	public void setStores(Map<String, Integer> stores) {
		this.stores = stores;
	}

	public String getTile() {
		return tile;
	}

	public void setTile(String tile) {
		this.tile = tile;
	}

	@Override
	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	@Override
	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
	}

	@Override
	public Long getTemplateId() {
		return getTemplate() != null ? getTemplate().getId() : null;
	}

	@Override
	public void setTemplateId(Long templateId) {
		// Not implemented
	}

	@Override
	public String getTemplateName() {
		return getTemplate() != null ? getTemplate().getName() : null;
	}

	@Override
	public void setTemplateName(String templateName) {
		// Not implemented
	}

	@Override
	public Template getTemplate() {
		return template;
	}

	@Override
	public void setTemplate(Template template) {
		this.template = template;
	}

	@Override
	public FolderAccessControlEntry getAccessControlEntry(long groupId) {
		return getAccessControlList().stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public Set<FolderAccessControlEntry> getAccessControlEntries(Set<Long> groupIds) {
		return getAccessControlList().stream().filter(ace -> groupIds.contains(ace.getGroupId()))
				.collect(Collectors.toSet());
	}

	@Override
	public void addAccessControlEntry(FolderAccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
	}

	@Override
	public int compareTo(Folder other) {
		if (this.equals(other))
			return 0;

		int comparison = Integer.compare(this.position, other.position);
		if (comparison != 0)
			return comparison;
		return this.name.compareTo(other.name);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + (int) (parentId ^ (parentId >>> 32));
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
		Folder other = (Folder) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return parentId == other.parentId;
	}
}