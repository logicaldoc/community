package com.logicaldoc.core.document;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.persistence.Cacheable;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.MapKeyColumn;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import org.hibernate.LazyInitializationException;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.exception.GenericJDBCException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.AccessControlEntry;
import com.logicaldoc.core.security.Secure;
import com.logicaldoc.core.util.IconSelector;
import com.logicaldoc.util.Context;

/**
 * Basic concrete implementation of {@link AbstractDocument}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
@Entity
@Table(name = "ld_document")
@Cacheable
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Document extends AbstractDocument implements Secure<DocumentAccessControlEntry> {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(Document.class);

	@ElementCollection
	@CollectionTable(name = "ld_document_acl", joinColumns = @JoinColumn(name = "ld_docid"))
	private Set<DocumentAccessControlEntry> accessControlList = new HashSet<>();

	@ElementCollection
	@CollectionTable(name = "ld_tag", joinColumns = @JoinColumn(name = "ld_docid"))
	@OrderBy("ld_tag")
	private Set<Tag> tags = new HashSet<>();

	@ElementCollection
	@MapKeyColumn(name = "ld_name", length = 255)
	@CollectionTable(name = "ld_document_ext", joinColumns = @JoinColumn(name = "ld_docid"))
	@OrderBy("ld_position ASC, ld_name ASC")
	private Map<String, Attribute> attributes = new HashMap<>();

	@ManyToOne()
	@JoinColumn(name = "ld_folderid")
	private Folder folder;

	@Column(name = "ld_deleteuser", length = 255)
	private String deleteUser;

	@Column(name = "ld_docref")
	private Long docRef;

	@Column(name = "ld_docreftype", length = 255)
	private String docRefType;

	@Column(name = "ld_rating")
	private Integer rating;

	@ManyToOne(cascade = javax.persistence.CascadeType.DETACH)
	@JoinColumn(name = "ld_templateid")
	private Template template;

	public Document() {
	}

	public Document(AbstractDocument source) {
		copyAttributes(source);
		setId(source.getId());
		setOcrd(source.getOcrd());
		setOcrTemplateId(source.getOcrTemplateId());
		setBarcoded(source.getBarcoded());
		setBarcodeTemplateId(source.getBarcodeTemplateId());
		setTemplate(source.getTemplate());
		setTemplateId(source.getTemplateId());
		setTemplateName(source.getTemplateName());

		if (source.getIndexed() != INDEX_INDEXED)
			setIndexed(source.getIndexed());
		setCustomId(null);

		try {
			if (source instanceof Document doc)
				for (DocumentAccessControlEntry ace : doc.getAccessControlList())
					getAccessControlList().add(new DocumentAccessControlEntry(ace));
		} catch (LazyInitializationException x) {
			// may happen do nothing
		}
	}

	@Override
	public Map<String, Attribute> getAttributes() {
		return attributes;
	}

	@Override
	public void setAttributes(Map<String, Attribute> attributes) {
		this.attributes = attributes;
	}

	public Integer getRating() {
		return rating;
	}

	public void setRating(Integer rating) {
		this.rating = rating;
	}

	public String getDocRefType() {
		return docRefType;
	}

	public void setDocRefType(String docRefType) {
		this.docRefType = docRefType;
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

	public String getDeleteUser() {
		return deleteUser;
	}

	public void setDeleteUser(String deleteUser) {
		this.deleteUser = deleteUser;
	}

	/**
	 * Retrieve the folder owning this document
	 * 
	 * @return the parent folder
	 */
	public Folder getFolder() {
		return folder;
	}

	public long getFolderId() {
		return folder != null ? folder.getId() : 0L;
	}

	public void setFolder(Folder folder) {
		this.folder = folder;
		if (folder != null)
			this.setTenantId(folder.getTenantId());
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

	@Override
	public void setTagsFromWords(Set<String> tgs) {
		super.setTagsFromWords(tgs);

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

	public String getTagsString() {
		StringBuilder sb = new StringBuilder(",");
		if (tags == null)
			return "";

		Iterator<Tag> iter = tags.iterator();
		boolean start = true;

		while (iter.hasNext()) {
			String words = iter.next().toString();
			words = words.replace(",", "\\,");

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

	@Override
	public String getIcon() {
		String icon = super.getIcon();

		if (docRef != null && docRef.longValue() != 0L) {
			if ("pdf".equals(getDocRefType()))
				icon = IconSelector.selectIcon("pdf");
			icon += "-shortcut";
		}

		return icon;
	}

	/**
	 * Copies in the current instance the attributes of the passed values
	 * object, but NOT the ID
	 * 
	 * @param docVO the document to get the attributes from
	 */
	@Override
	public void copyAttributes(AbstractDocument docVO) {
		super.copyAttributes(docVO);

		setTags(new HashSet<>());

		if (docVO instanceof Document document) {
			setDocRef(document.getDocRef());
			setFolder(document.getFolder());
			try {
				for (Tag tag : document.getTags())
					getTags().add(tag);
			} catch (GenericJDBCException | LazyInitializationException ex) {
				log.debug("Got error when trying to copy collections from document {}", docVO, ex);

				// load again the provided doc
				DocumentDAO docDao = Context.get(DocumentDAO.class);
				try {
					Document testDocVO = docDao.findById(docVO.getId());
					if (testDocVO != null) {
						docDao.initialize(testDocVO);
						for (Tag tag : testDocVO.getTags())
							getTags().add(tag);
					}
				} catch (PersistenceException e) {
					log.warn("Cannot copy collections from document {}", docVO, e);
				}
			}
		}
	}

	@Override
	public Template getTemplate() {
		return template;
	}

	@Override
	public void setTemplate(Template template) {
		this.template = template;
		if (template != null) {
			setTemplateId(template.getId());
			setTemplateName(template.getName());
		}
	}

	@Override
	public Set<DocumentAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	@Override
	public void setAccessControlList(Set<DocumentAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	@Override
	public AccessControlEntry getAccessControlEntry(long groupId) {
		return getAccessControlList().stream().filter(ace -> ace.getGroupId() == groupId).findFirst().orElse(null);
	}

	@Override
	public void addAccessControlEntry(DocumentAccessControlEntry ace) {
		if (!getAccessControlList().add(ace)) {
			getAccessControlList().remove(ace);
			getAccessControlList().add(ace);
		}
	}
}