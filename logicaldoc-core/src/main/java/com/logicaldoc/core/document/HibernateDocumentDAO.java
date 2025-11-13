package com.logicaldoc.core.document;

import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.sql.SqlUtil;

import jakarta.annotation.Resource;
import jakarta.transaction.Transactional;

/**
 * Hibernate implementation of <code>DocumentDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
@Repository("documentDAO")
@Transactional
public class HibernateDocumentDAO extends HibernatePersistentObjectDAO<Document> implements DocumentDAO {
	private static final String DOC_ID = "docId";

	private static final String TRANSACTION_CANNOT_BE_NULL = "transaction cannot be null";

	private static final String AND = " and ";

	private static final String AND_LD_TENANTID = " and ld_tenantid=";

	private static final String STATUS = ".status=";

	@Resource(name = "documentHistoryDAO")
	private DocumentHistoryDAO documentHistoryDAO;

	@Resource(name = "versionDAO")
	private VersionDAO versionDAO;

	@Resource(name = "tenantDAO")
	private TenantDAO tenantDAO;

	@Resource(name = "documentNoteDAO")
	private DocumentNoteDAO noteDAO;

	@Resource(name = "folderDAO")
	private FolderDAO folderDAO;

	@Resource(name = "userDAO")
	private UserDAO userDAO;

	@Resource(name = "groupDAO")
	private GroupDAO groupDAO;

	@Resource(name = "documentLinkDAO")
	private DocumentLinkDAO linkDAO;

	@Resource(name = "documentListenerManager")
	private DocumentListenerManager listenerManager;

	@Resource(name = "store")
	private Store store;

	@Resource(name = "ContextProperties")
	private ContextProperties config;

	private HibernateDocumentDAO() {
		super(Document.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentDAO.class);
	}

	@Override
	public void archive(long docId, DocumentHistory transaction) throws PersistenceException {
		Document doc = findById(docId);
		doc.setStatus(DocumentStatus.ARCHIVED);
		if (doc.getIndexed() != IndexingStatus.SKIP)
			doc.setIndexingStatus(IndexingStatus.TO_INDEX);
		doc.setLockUserId(transaction.getUserId());
		transaction.setEvent(DocumentEvent.ARCHIVED);
		store(doc, transaction);
		log.debug("Archived document {}", docId);
	}

	@Override
	public void unarchive(long docId, DocumentHistory transaction) throws PersistenceException {
		Document doc = findById(docId);
		doc.setStatus(DocumentStatus.UNLOCKED);
		doc.setLockUserId(null);
		transaction.setEvent(DocumentEvent.RESTORED);
		store(doc, transaction);
		log.debug("Unarchived document {}", docId);
	}

	@Override
	public void delete(long docId, DocumentHistory transaction) throws PersistenceException {
		delete(docId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public void delete(long docId, int delCode, DocumentHistory transaction) throws PersistenceException {
		if (delCode == 0)
			throw new IllegalArgumentException("delCode cannot be 0");

		if (transaction == null)
			throw new IllegalArgumentException(TRANSACTION_CANNOT_BE_NULL);
		if (transaction.getUser() == null)
			throw new IllegalArgumentException("transaction user cannot be null");

		if (!checkStoringAspect())
			return;

		Document doc = findById(docId);
		if (doc != null && doc.getImmutable() == 0
				|| (doc != null && doc.getImmutable() == 1 && transaction.getUser().isMemberOf(Group.GROUP_ADMIN))) {

			// Remove versions
			removeVersions(docId, delCode);

			// Remove notes
			removeNotes(docId, delCode);

			// Remove links
			removeLinks(docId, delCode);

			doc.setDeleted(delCode);
			doc.setDeleteUserId(transaction.getUserId());

			if (transaction.getUser() != null)
				doc.setDeleteUser(transaction.getUser().getFullName());
			else
				doc.setDeleteUser(transaction.getUsername());

			if (doc.getCustomId() != null)
				doc.setCustomId(doc.getCustomId() + "." + doc.getId());
			store(doc, transaction);
		}
	}

	private void removeLinks(long docId, int delCode) throws PersistenceException {
		for (DocumentLink link : linkDAO.findByDocId(docId)) {
			link.setDeleted(delCode);
			saveOrUpdate(link);
		}
	}

	private void removeNotes(long docId, int delCode) throws PersistenceException {
		for (DocumentNote note : noteDAO.findByDocId(docId, User.USERID_ADMIN, null)) {
			note.setDeleted(delCode);
			saveOrUpdate(note);
		}
	}

	private void removeVersions(long docId, int delCode) throws PersistenceException {
		for (Version version : versionDAO.findByDocId(docId)) {
			version.setDeleted(delCode);
			saveOrUpdate(version);
		}
	}

	@Override
	public List<Long> findByUserId(long userId) throws PersistenceException {
		Collection<Folder> folders = folderDAO.findByUserId(userId);
		if (folders.isEmpty())
			return new ArrayList<>();

		StringBuilder query = new StringBuilder();
		query.append(ENTITY + ".folder.id in (");
		boolean first = true;
		for (Folder folder : folders) {
			if (!first)
				query.append(",");
			query.append(folder.getId());
			first = false;
		}
		query.append(") and not " + ENTITY + STATUS + DocumentStatus.ARCHIVED.ordinal());
		return findIdsByWhere(query.toString(), null, null);
	}

	@Override
	public List<Document> findByLockUserAndStatus(Long userId, DocumentStatus status) {
		StringBuilder sb = new StringBuilder(
				"select ld_id, ld_folderid, ld_version, ld_fileversion, ld_lastmodified, ld_filename from ld_document where ld_deleted = 0 ");
		if (userId != null)
			sb.append(" and ld_lockuserid=" + userId);

		if (status != null)
			sb.append(" and ld_status=" + status.ordinal());

		try {
			return query(sb.toString(), (resultSet, col) -> {
				Document doc = new Document();
				doc.setId(resultSet.getLong(1));
				Folder folder = new Folder();
				folder.setId(resultSet.getLong(2));
				doc.setFolder(folder);
				doc.setVersion(resultSet.getString(3));
				doc.setFileVersion(resultSet.getString(4));
				doc.setLastModified(resultSet.getTimestamp(5));
				doc.setFileName(resultSet.getString(6));
				return doc;
			}, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public List<Long> findDocIdByTag(String tag) throws PersistenceException {
		StringBuilder query = new StringBuilder(
				"select distinct(A.ld_docid) from ld_tag A, ld_document B where A.ld_docid=B.ld_id and not B.ld_status="
						+ DocumentStatus.ARCHIVED.ordinal());
		query.append(" and lower(ld_tag)='" + SqlUtil.doubleQuotesAndBackslashes(tag).toLowerCase() + "'");
		return queryForList(query.toString(), Long.class);
	}

	@Override
	public void store(final Document doc) throws PersistenceException {
		store(doc, null);
	}

	@Override
	public void store(Document doc, final DocumentHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		try {
			Tenant tenant = tenantDAO.findById(doc.getTenantId());

			if (transaction != null) {
				transaction.setTenantId(doc.getTenantId());
				transaction.setTenant(tenant.getName());
			}

			truncatePublishingDates(doc);

			setIndexed(doc, tenant);

			setTags(doc);

			removeForbiddenPermissionsForGuests(doc);

			setType(doc);

			// Remove the sections
			doc.getAttributes().values().removeIf(Attribute::isSection);

			// Count those attributes that reference other documents
			doc.setDocAttrs((int) doc.getAttributes().values().stream().filter(
					a -> a.getType() == Attribute.TYPE_DOCUMENT && a.getIntValue() != null && a.getIntValue() != 0L)
					.count());

			/*
			 * Avoid documents inside folder alias
			 */
			setFolder(doc);

			if (doc.getDocRef() == null) {
				/*
				 * In case of a regular document, check for attributes defaults
				 * specified at folder's level
				 */
				copyFolderMetadata(doc);
			}

			if (!RunLevel.current().aspectEnabled("customId")) {
				doc.setCustomId(UUID.randomUUID().toString());
				log.debug("Aspect customId is disabled so force the the Custom ID to a random UUID");
			}

			/*
			 * Check maximum number of documents per folder
			 */
			checkMaxDocsPerFolder(doc);

			log.debug("Invoke listeners before store");
			Map<String, Object> dictionary = new HashMap<>();
			for (DocumentListener listener : listenerManager.getListeners())
				listener.beforeStore(doc, transaction, dictionary);

			if (StringUtils.isEmpty(doc.getCustomId()))
				doc.setCustomId(UUID.randomUUID().toString());

			if (StringUtils.isEmpty(doc.getRevision()))
				doc.setRevision(doc.getVersion());

			// Use unique filename in the same folder
			setUniqueFilename(doc);

			// Save the document
			saveOrUpdate(doc);

			if (doc.getDeleted() == 0 && doc.getId() != 0L) {
				// Take the document again in order to retrieve the updated
				// persisted instance.
				doc = findById(doc.getId());
				initialize(doc);
			}

			doc.setModified(false);

			log.debug("Invoke listeners after store");
			for (DocumentListener listener : listenerManager.getListeners())
				listener.afterStore(doc, transaction, dictionary);

			if (StringUtils.isEmpty(doc.getCustomId())) {
				doc.setCustomId(Long.toString(doc.getId()));
				doc.setModified(true);
			}

			if (StringUtils.isEmpty(doc.getRevision())) {
				doc.setRevision(doc.getVersion());
				doc.setModified(true);
			}

			// Perhaps some listeners may have modified the document
			if (doc.isModified())
				saveOrUpdate(doc);
			saveDocumentHistory(doc, transaction, dictionary);

			/**
			 * Update the aliases
			 */
			updateAliases(doc);
		} catch (Exception e) {
			handleStoreError(transaction, e);
		}
	}

	private void removeForbiddenPermissionsForGuests(Document document) throws PersistenceException {
		// Remove the forbidden permissions for the guests
		for (DocumentAccessControlEntry ace : document.getAccessControlList()) {
			Group group = groupDAO.findById(ace.getGroupId());
			if (group != null && group.isGuest()) {
				ace.setArchive(0);
				ace.setAutomation(0);
				ace.setCalendar(0);
				ace.setDelete(0);
				ace.setImmutable(0);
				ace.setMove(0);
				ace.setPassword(0);
				ace.setRename(0);
				ace.setSecurity(0);
				ace.setSign(0);
				ace.setWorkflow(0);
				ace.setWrite(0);
			}
		}
	}

	private void checkMaxDocsPerFolder(Document document) throws PersistenceException {
		long maxDocsPerFolder = config.getLong("maxdocsperfolder", -1L);
		if (document.getId() == 0L && maxDocsPerFolder > 0) {
			long count = folderDAO.countDocs(document.getFolder().getId());
			if (count >= maxDocsPerFolder)
				throw new TooManyDocumentsException(document.getFolder(), maxDocsPerFolder);
		}
	}

	private void setType(Document doc) {
		if (StringUtils.isEmpty(doc.getType()) && doc.getFileName().contains("."))
			doc.setType(FileUtil.getExtension(doc.getFileName()).toLowerCase());
	}

	private boolean handleStoreError(final DocumentHistory transaction, Throwable e) throws PersistenceException {
		if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
			Session session = SessionManager.get().get(transaction.getSessionId());
			if (session != null)
				session.logError(e.getMessage());
		}
		log.error(e.getMessage(), e);

		if (e instanceof PersistenceException pe)
			throw pe;
		else
			throw new PersistenceException(e);
	}

	private void updateAliases(Document doc) throws PersistenceException {
		if (doc.getDocRef() == null)
			jdbcUpdate("update ld_document set ld_filesize= " + doc.getFileSize() + ", ld_pages= " + doc.getPages()
					+ ", ld_version='" + doc.getVersion() + "', ld_fileversion='" + doc.getFileVersion()
					+ "' where ld_docref= " + doc.getId());
	}

	private void copyFolderMetadata(Document doc) {
		if (doc.getFolder().getTemplate() != null)
			copyFolderExtendedAttributes(doc);
		/*
		 * Check for OCR template at folder level
		 */
		if (doc.getOcrTemplateId() == null && doc.getFolder().getOcrTemplateId() != null)
			doc.setOcrTemplateId(doc.getFolder().getOcrTemplateId());

		/*
		 * Check for Barcode template at folder level
		 */
		if (doc.getBarcodeTemplateId() == null && doc.getFolder().getBarcodeTemplateId() != null)
			doc.setBarcodeTemplateId(doc.getFolder().getBarcodeTemplateId());

		if (doc.getTemplate() == null)
			doc.setOcrTemplateId(null);
	}

	private void copyFolderExtendedAttributes(Document doc) {
		folderDAO.initialize(doc.getFolder());
		if (doc.getTemplate() == null || doc.getTemplate().equals(doc.getFolder().getTemplate())) {
			doc.setTemplate(doc.getFolder().getTemplate());
			for (String name : doc.getFolder().getAttributeNames()) {
				Attribute fAtt = doc.getFolder().getAttribute(name);
				if (fAtt.getValue() == null || StringUtils.isEmpty(fAtt.getValue().toString()))
					continue;
				Attribute dAtt = doc.getAttribute(name);
				if (dAtt == null) {
					dAtt = new Attribute();
					dAtt.setType(fAtt.getType());
					dAtt.setEditor(fAtt.getEditor());
					dAtt.setLabel(fAtt.getLabel());
					dAtt.setMandatory(fAtt.getMandatory());
					dAtt.setHidden(fAtt.getHidden());
					dAtt.setMultiple(fAtt.getMultiple());
					dAtt.setPosition(fAtt.getPosition());
					doc.getAttributes().put(name, dAtt);
				}

				if (dAtt.getValue() == null || StringUtils.isEmpty(dAtt.getValue().toString())) {
					dAtt.setStringValue(fAtt.getStringValue());
					dAtt.setDateValue(fAtt.getDateValue());
					dAtt.setDoubleValue(fAtt.getDoubleValue());
					dAtt.setIntValue(fAtt.getIntValue());
				}
			}
		}
	}

	private void setFolder(Document doc) throws PersistenceException {
		if (doc.getFolder().getFoldRef() != null) {
			Folder fld = folderDAO.findById(doc.getFolder().getFoldRef());
			if (fld == null)
				throw new PersistenceException(
						String.format("Unable to find refrenced folder %s", doc.getFolder().getFoldRef()));
			folderDAO.initialize(fld);
			doc.setFolder(fld);
		}
	}

	private void setTags(Document doc) {
		Set<Tag> src = doc.getTags();
		if (CollectionUtils.isNotEmpty(src)) {
			// Trim too long tags
			Set<Tag> dst = new HashSet<>();
			for (Tag str : src) {
				str.setTenantId(doc.getTenantId());
				String s = str.getTag();
				if (s != null) {
					if (s.length() > 255) {
						s = s.substring(0, 255);
						str.setTag(s);
					}
					if (!dst.contains(str))
						dst.add(str);
				}
			}
			doc.setTags(dst);
			doc.setTgs(doc.getTagsString());
		}
	}

	private void setIndexed(Document doc, Tenant tenant) {
		if (doc.getIndexed() == IndexingStatus.TO_INDEX || doc.getIndexed() == IndexingStatus.TO_INDEX_METADATA) {
			// Check if the document must be indexed
			if (!FileUtil.matches(doc.getFileName(), config.getProperty(tenant.getName() + ".index.includes", ""),
					config.getProperty(tenant.getName() + ".index.excludes", "")))
				doc.setIndexingStatus(IndexingStatus.SKIP);

			// Check if the document must be indexed
			if (doc.getIndexed() == IndexingStatus.SKIP && FileUtil.matches(doc.getFileName(),
					config.getProperty(tenant.getName() + ".index.includes.metadata", ""),
					config.getProperty(tenant.getName() + ".index.excludes.metadata", "")))
				doc.setIndexingStatus(IndexingStatus.TO_INDEX_METADATA);
		}
	}

	private void truncatePublishingDates(Document doc) {
		// Truncate publishing dates
		if (doc.getStartPublishing() != null) {
			Instant instant = doc.getStartPublishing().toInstant();
			ZonedDateTime zonedDateTime = instant.atZone(ZoneId.systemDefault());
			ZonedDateTime truncatedZonedDateTime = zonedDateTime.truncatedTo(ChronoUnit.DAYS);
			Instant truncatedInstant = truncatedZonedDateTime.toInstant();
			doc.setStartPublishing(Date.from(truncatedInstant));
		}

		if (doc.getStopPublishing() != null) {
			Instant instant = doc.getStopPublishing().toInstant();
			ZonedDateTime zonedDateTime = instant.atZone(ZoneId.systemDefault());
			ZonedDateTime truncatedZonedDateTime = zonedDateTime.truncatedTo(ChronoUnit.DAYS);
			Instant truncatedInstant = truncatedZonedDateTime.toInstant();
			doc.setStopPublishing(Date.from(truncatedInstant));
		}
	}

	/**
	 * Avoid file name duplications in the same folder
	 * 
	 * @throws PersistenceException Error in the data layer
	 */
	private void setUniqueFilename(Document doc) throws PersistenceException {
		if (!RunLevel.current().aspectEnabled("uniquenessFilename"))
			return;

		String baseName = doc.getFileName();
		String ext = "";
		if (doc.getFileName().indexOf(".") != -1) {
			baseName = FileUtil.getBaseName(doc.getFileName());
			ext = "." + FileUtil.getExtension(doc.getFileName());
		}

		/*
		 * These sets will contain the found collisions in the given folder
		 */
		final Set<String> fileNames = new HashSet<>();

		StringBuilder query = new StringBuilder(
				"select ld_filename from ld_document where ld_deleted=0 and ld_folderid=");
		query.append(Long.toString(doc.getFolder().getId()));
		query.append(" and ld_filename like '");
		query.append(SqlUtil.doubleQuotes(baseName));
		query.append("%' and not ld_id=");
		query.append(Long.toString(doc.getId()));

		// Execute the query to populate the sets
		queryForResultSet(query.toString(), null, null, rs -> {
			while (rs.next()) {
				String file = rs.getString(1);
				if (file != null && !fileNames.contains(file))
					fileNames.add(file.toLowerCase());
			}
		});

		int counter = 1;
		while (fileNames.contains(doc.getFileName().toLowerCase()))
			doc.setFileName(baseName + "(" + (counter++) + ")" + ext);
	}

	@Override
	public void updateDigest(Document doc) throws PersistenceException {
		StoreResource resource=StoreResource.builder().document(doc).build();
		if (store.exists(resource)) {
			try (InputStream in = store.getStream(resource);) {
				doc.setDigest(FileUtil.computeDigest(in));
			} catch (IOException e) {
				log.error("Cannot retrieve the content of document {}", doc);
				log.error(e.getMessage(), e);
			}

			saveOrUpdate(doc);
			flush();
			evict(doc);

			// Update the versions also
			Map<String, Object> params = new HashMap<>();
			params.put("fileVersion", doc.getFileVersion());
			params.put("digest", doc.getDigest());
			params.put(DOC_ID, doc.getId());
			jdbcUpdate(
					"update ld_version set ld_digest = :digest  where ld_documentid = :docId and ld_fileversion = :fileVersion",
					params);
		}
	}

	public List<Document> findLastModifiedByUserId(long userId, int maxElements) throws PersistenceException {
		List<Document> coll = new ArrayList<>();

		StringBuilder query = new StringBuilder("SELECT _history.docId from DocumentHistory _history");
		query.append(" WHERE _history.userId = " + Long.toString(userId) + " ");
		query.append(" ORDER BY _history.date DESC");

		List<Long> results = new ArrayList<>();
		try {
			results = findByQuery(query.toString(), (Map<String, Object>) null, Long.class, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		for (Long docid : results) {
			if (coll.size() >= maxElements)
				break;
			if (docid != null) {
				Document document = findById(docid);
				if (folderDAO.isReadAllowed(document.getFolder().getId(), userId))
					coll.add(document);
			}
		}

		return coll;
	}

	public List<String> findTags(long docId) throws PersistenceException {
		return queryForList("select ld_tag from ld_tag where ld_docid=" + docId + " order by ld_tag", String.class);
	}

	@Override
	public Map<String, Long> findTags(String firstLetter, Long tenantId) throws PersistenceException {
		final Map<String, Long> map = new HashMap<>();

		StringBuilder query = new StringBuilder("SELECT ld_count, ld_tag from ld_uniquetag where 1=1 ");
		if (StringUtils.isNotEmpty(firstLetter))
			query.append(" and lower(ld_tag) like '" + firstLetter.toLowerCase() + "%' ");
		if (tenantId != null)
			query.append(AND_LD_TENANTID + tenantId);

		query(query.toString(), new RowMapper<Object>() {

			@Override
			public Object mapRow(ResultSet rs, int rowNumber) throws SQLException {
				Long value = rs.getLong(1);
				String key = rs.getString(2);
				map.put(key, value);
				return null;
			}

		}, null);

		return map;
	}

	@Override
	public List<String> findAllTags(String firstLetter, Long tenantId) throws PersistenceException {
		StringBuilder sb = new StringBuilder("select ld_tag from ld_uniquetag where 1=1 ");
		if (tenantId != null) {
			sb.append(AND_LD_TENANTID + tenantId);
		}

		Map<String, Object> params = new HashMap<>();
		if (firstLetter != null) {
			sb.append(" and lower(ld_tag) like :tagLike ");
			params.put("tagLike", firstLetter.toLowerCase() + "%");
		}

		return queryForList(sb.toString(), params, String.class, null);
	}

	@Override
	public List<Document> findByUserIdAndTag(long userId, String tag, Integer max) throws PersistenceException {
		List<Document> coll = new ArrayList<>();

		List<Long> ids = findDocIdByUserIdAndTag(userId, tag);
		if (!ids.isEmpty()) {
			StringBuilder query = new StringBuilder("select A from Document A where A.id in (");
			query.append(ids.stream().map(Object::toString).collect(Collectors.joining(",")));
			query.append(")");
			try {
				coll = findByObjectQuery(query.toString(), (Map<String, Object>) null, max);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return coll;
	}

	@Override
	public List<Long> findDocIdByUserIdAndTag(long userId, String tag) throws PersistenceException {
		List<Long> ids = new ArrayList<>();

		User user = userDAO.findById(userId);
		if (user == null)
			return ids;

		StringBuilder query = new StringBuilder();

		if (user.isMemberOf(Group.GROUP_ADMIN)) {
			ids = findDocIdByTag(tag);
		} else {

			/*
			 * Search for all accessible folders
			 */
			Collection<Long> precoll = folderDAO.findFolderIdByUserId(userId, null, true);
			String precollString = precoll.toString().replace('[', '(').replace(']', ')');

			query.append("select distinct(C.ld_id) from ld_document C, ld_tag D "
					+ " where C.ld_id=D.ld_docid AND C.ld_deleted=0 and not C.ld_status="
					+ DocumentStatus.ARCHIVED.ordinal());
			query.append(" AND C.ld_folderid in ");
			query.append(precollString);
			query.append(" AND lower(D.ld_tag)='" + SqlUtil.doubleQuotes(tag.toLowerCase()) + "' ");

			log.debug("Find by tag: {}", query);
			ids.addAll(queryForList(query.toString(), Long.class));
		}

		return ids;
	}

	@Override
	public List<Document> findLastDownloadsByUserId(long userId, int maxResults) throws PersistenceException {
		List<Document> coll = new ArrayList<>();

		StringBuilder query = new StringBuilder("select docId from DocumentHistory ");
		query.append(" where userId = " + userId);
		query.append(" and event = '" + DocumentEvent.DOWNLOADED + "' ");
		query.append(" order by date desc");

		ArrayList<Long> tmpal = new ArrayList<>(
				findByQuery(query.toString(), (Map<String, Object>) null, Long.class, null));
		List<Long> docIds = tmpal;

		if (docIds.isEmpty())
			return coll;

		if (docIds.size() > maxResults) {
			tmpal.subList(0, maxResults - 1);
		}

		query = new StringBuilder("from Document " + ENTITY + " ");
		query.append(" where not " + ENTITY + STATUS + DocumentStatus.ARCHIVED.ordinal());
		query.append(AND + ENTITY + ".id in (");

		for (int i = 0; i < docIds.size(); i++) {
			Long docId = docIds.get(i);
			if (i > 0)
				query.append(",");
			query.append(docId);
		}
		query.append(")");

		// execute the query
		List<Document> unorderdColl = findByQuery(query.toString(), (Map<String, Object>) null, Document.class, null);

		// put all elements in a map
		HashMap<Long, Document> hm = new HashMap<>();
		for (Document doc : unorderdColl) {
			hm.put(doc.getId(), doc);
		}

		// Access the map using the folderIds
		// if a match is found, put it in the original list
		for (Long docId : docIds) {
			Document myDoc = hm.get(docId);
			if (myDoc != null)
				coll.add(myDoc);
		}

		return coll;
	}

	@Override
	public List<Long> findDocIdByFolder(long folderId, Integer max) throws PersistenceException {
		String sql = "select ld_id from ld_document where ld_deleted=0 and ld_folderid = " + folderId
				+ " and not ld_status=" + DocumentStatus.ARCHIVED.ordinal();
		return queryForList(sql, null, Long.class, max);
	}

	@Override
	public List<Document> findByFolder(long folderId, Integer max) throws PersistenceException {
		Map<String, Object> params = new HashMap<>();
		params.put("folderId", Long.valueOf(folderId));
		return findByWhere(ENTITY + ".folder.id = :folderId ", params, null, max);
	}

	@Override
	public List<Document> findArchivedByFolder(long folderId) throws PersistenceException {
		return findByWhere(
				ENTITY + ".folder.id = " + folderId + AND + ENTITY + STATUS + DocumentStatus.ARCHIVED.ordinal(), null,
				null);
	}

	@Override
	public List<Document> findLinkedDocuments(long docId, String linkType, Integer direction)
			throws PersistenceException {
		StringBuilder query = new StringBuilder("");
		if (direction == null)
			query.append(
					"select distinct(ld_docid2) from ld_link where ld_deleted=0 and (ld_docid1 = :docId) UNION select distinct(ld_docid1) from ld_link where ld_deleted=0 and (ld_docid2 = :docId)");
		else if (direction.intValue() == 1)
			query.append("select distinct(ld_docid2) from ld_link where ld_deleted=0 and (ld_docid1 = :docId)");
		else if (direction.intValue() == 2)
			query.append("select distinct(ld_docid1) from ld_link where ld_deleted=0 and (ld_docid2 = :docId)");

		Map<String, Object> params = new HashMap<>();
		params.put(DOC_ID, docId);

		List<Long> ids = queryForList(query.toString(), params, Long.class, null);

		if (ids.isEmpty())
			return new ArrayList<>();
		else
			return findByWhere(ENTITY + ".id in (" + ids.stream().map(Object::toString).collect(Collectors.joining(","))
					+ ") and not " + ENTITY + STATUS + DocumentStatus.ARCHIVED.ordinal(), null, null);
	}

	@Override
	public List<Document> findByFileNameAndParentFolderId(Long folderId, String fileName, Long excludeId, Long tenantId,
			Integer max) throws PersistenceException {
		String query = "lower(" + ENTITY + ".fileName) like '%" + SqlUtil.doubleQuotes(fileName.toLowerCase()) + "%'";
		if (tenantId != null) {
			query += AND + ENTITY + ".tenantId = " + tenantId;
		}
		if (folderId != null) {
			query += AND + ENTITY + ".folder.id = " + folderId;
		}
		if (excludeId != null)
			query += " and not(" + ENTITY + ".id = " + excludeId + ")";
		query += " and not " + ENTITY + STATUS + DocumentStatus.ARCHIVED.ordinal();

		return findByWhere(query, null, max);
	}

	@Override
	public void initialize(Document doc) {
		if (doc == null)
			return;

		refresh(doc);

		if (doc.getAttributes() != null)
			log.trace("Initialized {} attributes", doc.getAttributes().keySet().size());

		if (doc.getTags() != null)
			log.trace("Initialized {} tags", doc.getTags().size());

		if (doc.getAccessControlList() != null)
			log.trace("Initialized {} aces", doc.getAccessControlList().size());
	}

	@Override
	public List<Long> findDeletedDocIds() throws PersistenceException {
		String query = "select ld_id from ld_document where ld_deleted=1 order by ld_lastmodified desc";
		return queryForList(query, Long.class);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Document> findDeletedDocs() throws PersistenceException {
		String query = "select ld_id, ld_customid, ld_lastModified, ld_filename from ld_document where ld_deleted=1 order by ld_lastmodified desc";

		@SuppressWarnings("rawtypes")
		RowMapper docMapper = new BeanPropertyRowMapper() {
			@Override
			public Object mapRow(ResultSet rs, int rowNum) throws SQLException {
				Document doc = new Document();
				doc.setId(rs.getLong(1));
				doc.setCustomId(rs.getString(2));
				doc.setLastModified(rs.getDate(3));
				doc.setFileName(rs.getString(4));
				return doc;
			}
		};

		return query(query, docMapper, null);
	}

	@Override
	public long computeTotalSize(Long tenantId, Long userId, boolean computeDeleted) throws PersistenceException {
		// Count all the versions of the documents related to file change
		final String query = "select sum(ld_filesize) from ld_version where ld_version = ld_fileversion"
				+ (computeDeleted ? "" : " and ld_deleted=0 ") + (userId != null ? " and ld_publisherid=" + userId : "")
				+ (tenantId != null ? AND_LD_TENANTID + tenantId : "");
		return queryForLong(query);
	}

	@Override
	public long count(Long tenantId, boolean computeDeleted, boolean computeArchived) throws PersistenceException {
		String query = "select count(*) from ld_document where 1=1 ";
		if (!computeDeleted)
			query += " and ld_deleted = 0 ";
		if (!computeArchived)
			query += " and not ld_status = " + DocumentStatus.ARCHIVED.ordinal();
		if (tenantId != null)
			query += " and ld_tenantid = " + tenantId;

		return queryForLong(query);
	}

	@Override
	public List<Document> findByIndexingStatus(IndexingStatus indexingStatus) throws PersistenceException {
		return findByWhere(ENTITY + ".docRef is null and " + ENTITY + ".indexingStatus = " + indexingStatus.ordinal(),
				ENTITY + ".lastModified asc", null);
	}

	@Override
	public void restore(long docId, long folderId, final DocumentHistory transaction) throws PersistenceException {

		// Update the document using HQL
		bulkUpdate("set deleted=0, lastModified=CURRENT_TIMESTAMP where id = :docId", Map.of(DOC_ID, docId));
		bulkUpdate("set folder = :folder where id = :docId",
				Map.of("folder", getCurrentSession().get(Folder.class, folderId), DOC_ID, docId));

		// Update the version using HQL
		versionDAO.bulkUpdate("set deleted=0, lastModified=CURRENT_TIMESTAMP where id = :docId", Map.of(DOC_ID, docId));
		versionDAO.bulkUpdate("set folderId = :folderId where id = :docId",
				Map.of("folderId", folderId, DOC_ID, docId));

		Document doc = findById(docId);
		if (doc != null && transaction != null) {
			transaction.setDocId(docId);
			transaction.setEvent(DocumentEvent.RESTORED);

			initialize(doc);
			store(doc, transaction);
		}
	}

	@Override
	public Document findByCustomId(String customId, long tenantId) throws PersistenceException {
		Document doc = null;
		if (customId != null) {
			String query = ENTITY + ".customId = '" + SqlUtil.doubleQuotes(customId) + "' " + AND + ENTITY
					+ ".tenantId=" + tenantId;
			List<Document> coll = findByWhere(query, null, null);
			if (!coll.isEmpty()) {
				doc = coll.get(0);
				if (doc.getDeleted() == 1)
					doc = null;
			}
		}
		return doc;
	}

	@Override
	public void makeImmutable(long docId, DocumentHistory transaction) throws PersistenceException {
		Document doc = findById(docId);
		initialize(doc);
		doc.setImmutable(1);
		doc.setStatus(DocumentStatus.UNLOCKED);
		store(doc, transaction);
	}

	@Override
	public void deleteAll(Collection<Document> documents, DocumentHistory transaction) throws PersistenceException {
		deleteAll(documents, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public void deleteAll(Collection<Document> documents, int delCode, DocumentHistory transaction)
			throws PersistenceException {
		for (Document document : documents) {
			DocumentHistory deleteHistory = new DocumentHistory(transaction);
			deleteHistory.setEvent(DocumentEvent.DELETED);
			delete(document.getId(), delCode, deleteHistory);
		}
	}

	@Override
	public void saveDocumentHistory(Document doc, DocumentHistory transaction) throws PersistenceException {
		Map<String, Object> dictionary = new HashMap<>();
		saveDocumentHistory(doc, transaction, dictionary);
	}

	private void saveDocumentHistory(Document doc, DocumentHistory transaction, Map<String, Object> dictionary)
			throws PersistenceException {
		if (doc == null || transaction == null || !RunLevel.current().aspectEnabled("saveHistory"))
			return;

		try {
			transaction.setTenantId(doc.getTenantId());
			transaction.setDocId(doc.getId());
			transaction.setFolderId(doc.getFolder().getId());
			transaction.setVersion(doc.getVersion());
			transaction.setFileVersion(doc.getFileVersion());
			transaction.setRevision(doc.getRevision());
			transaction.setFilename(doc.getFileName());
			transaction.setFileSize(doc.getFileSize());
			transaction.setNotified(0);
			transaction.setDocument(doc);
			transaction.setPath(folderDAO.computePathExtended(doc.getFolder().getId()));

			documentHistoryDAO.store(transaction);

			log.debug("Invoke listeners after store");
			for (DocumentListener listener : listenerManager.getListeners())
				listener.afterSaveHistory(doc, transaction, dictionary);

			EventCollector.get().newEvent(transaction);
		} catch (PersistenceException e) {
			if (StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			if (e instanceof PersistenceException)
				throw e;
			else
				throw new PersistenceException(e);
		}
	}

	@Override
	public long countByIndexed(IndexingStatus indexingStatus) throws PersistenceException {
		return queryForLong(
				"select count(*) from ld_document where ld_deleted=0 and ld_indexed = " + indexingStatus.ordinal());
	}

	@Override
	public List<Long> findAliasIds(long docId) throws PersistenceException {
		return findIdsByWhere(ENTITY + ".docRef = " + Long.toString(docId), null, null);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Document> findDeleted(long userId, Integer maxHits) throws PersistenceException {
		String query = "select ld_id, ld_lastmodified, ld_filename, ld_customid, ld_tenantid, ld_folderid, ld_color from ld_document where ld_deleted=1 and ld_deleteuserid = "
				+ userId + " order by ld_lastmodified desc";

		@SuppressWarnings("rawtypes")
		RowMapper docMapper = new BeanPropertyRowMapper() {
			@Override
			public Object mapRow(ResultSet rs, int rowNum) throws SQLException {
				Document doc = new Document();
				doc.setId(rs.getLong(1));
				doc.setLastModified(rs.getTimestamp(2));
				doc.setFileName(rs.getString(3));
				doc.setCustomId(rs.getString(4));
				doc.setTenantId(rs.getLong(5));

				Folder folder = new Folder();
				folder.setId(rs.getLong(6));
				doc.setFolder(folder);

				doc.setColor(rs.getString(7));
				return doc;
			}
		};

		return query(query, docMapper, maxHits);
	}

	@Override
	public List<Document> findByIds(Set<Long> ids, Integer max) {
		List<Document> docs = new ArrayList<>();
		if (CollectionUtils.isEmpty(ids))
			return docs;

		try {
			docs = findByWhere(ENTITY + ".id in("
					+ ids.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")) + ")", null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return docs;
	}

	@Override
	public void deleteOrphaned(long deleteUserId) throws PersistenceException {
		String dbms = config.getProperty("jdbc.dbms") != null ? config.getProperty("jdbc.dbms").toLowerCase() : "mysql";

		String concat = "CONCAT(ld_id,CONCAT('.',ld_customid))";
		if (dbms.contains("postgre"))
			concat = "ld_id || '.' || ld_customid";
		if (dbms.contains("mssql"))
			concat = "CAST(ld_id AS varchar) + '.' + ld_customid";

		jdbcUpdate("update ld_document set ld_deleted=1,ld_customid=" + concat + ", ld_deleteuserid=" + deleteUserId
				+ " where ld_deleted=0 and ld_folderid in (select ld_id from ld_folder where ld_deleted  > 0)");
	}

	@Override
	public Collection<Long> findPublishedIds(Collection<Long> folderIds) throws PersistenceException {
		StringBuilder query = new StringBuilder("select ld_id from ld_document where ld_deleted=0 and not ld_status="
				+ DocumentStatus.ARCHIVED.ordinal());
		if (folderIds != null && !folderIds.isEmpty()) {
			query.append(" and ld_folderid in (");
			query.append(folderIds.toString().replace('[', ' ').replace(']', ' '));
			query.append(" ) ");
		}
		query.append(" and ld_published = 1 ");
		query.append(" and ld_startpublishing <= :now ");
		query.append(" and ( ld_stoppublishing is null or ld_stoppublishing > :now )");

		Map<String, Object> params = new HashMap<>();
		params.put("now", new Date());

		Collection<Long> buf = queryForList(query.toString(), params, Long.class, null);
		Set<Long> ids = new HashSet<>();
		for (Long id : buf) {
			if (!ids.contains(id))
				ids.add(id);
		}
		return ids;
	}

	@Override
	public void cleanExpiredTransactions() throws PersistenceException {
		// Retrieve the actual registered locks on transactions
		List<String> transactionIds = queryForList(
				"select ld_string1 from ld_generic where ld_type='lock' and ld_string1 is not null", String.class);
		String transactionIdsStr = transactionIds.toString().replace("[", "('").replace("]", "')").replace(", ", "','");

		bulkUpdate(
				"set transactionId=null where transactionId is not null and transactionId not in " + transactionIdsStr,
				(Map<String, Object>) null);
	}

	@Override
	public void cleanUnexistingUniqueTags() throws PersistenceException {
		try {
			StringBuilder deleteStatement = new StringBuilder("DELETE FROM ld_uniquetag WHERE ");

			// tags no more existing in the ld_tag table or that belong to
			// deleted documents
			deleteStatement.append(
					" ld_uniquetag.ld_tag NOT IN ( SELECT DISTINCT t.ld_tag FROM ld_tag t JOIN ld_document d ON d.ld_id = t.ld_docid WHERE ld_uniquetag.ld_tenantid = t.ld_tenantid AND ld_uniquetag.ld_tag = t.ld_tag AND d.ld_deleted = 0 ) ");

			// tags no more existing in the ld_foldertag table or that belong to
			// deleted folders
			deleteStatement.append(
					" AND ld_uniquetag.ld_tag NOT IN ( SELECT DISTINCT ft.ld_tag FROM ld_foldertag ft JOIN ld_folder f ON f.ld_id = ft.ld_folderid WHERE ld_uniquetag.ld_tenantid = ft.ld_tenantid AND ld_uniquetag.ld_tag = ft.ld_tag AND f.ld_deleted = 0 ); ");

			jdbcUpdate(deleteStatement.toString());
		} catch (PersistenceException e) {
			/*
			 * The unique SQL query failed, so we do the same deletion
			 * programmatically one by one
			 */
			cleanUnexistingUniqueTagsOneByOne();
		}
	}

	@Override
	public void cleanUnexistingUniqueTagsOneByOne() throws PersistenceException {
		List<Long> tenantIds = tenantDAO.findAllIds();
		for (Long tenantId : tenantIds) {
			log.debug("Clean unique tags of tenant {}", tenantId);

			// Collect the currently unique used tags
			Set<String> currentlyUsedTags = (queryForList(
					"select distinct(B.ld_tag) from ld_tag B, ld_document C where B.ld_tenantid=" + tenantId
							+ " and C.ld_id=B.ld_docid and C.ld_deleted=0 "
							+ " UNION select distinct(D.ld_tag) from ld_foldertag D, ld_folder E where D.ld_tenantid="
							+ tenantId + " and E.ld_id=D.ld_folderid and E.ld_deleted=0",
					String.class).stream().collect(Collectors.groupingBy(Function.identity()))).keySet();

			// Delete all currently recorded unique tags no more used
			if (isOracle()) {
				/*
				 * In Oracle the limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				String currentlyUsedTagsStr = currentlyUsedTags.stream()
						.map(tag -> ("('" + SqlUtil.doubleQuotes(tag) + "',0)")).collect(Collectors.joining(","));
				if (StringUtils.isNotEmpty(currentlyUsedTagsStr))
					jdbcUpdate("delete from ld_uniquetag where ld_tenantid=" + tenantId + " and (ld_tag,0) not in ("
							+ currentlyUsedTagsStr + ")");
			} else {
				String currentlyUsedTagsStr = currentlyUsedTags.stream()
						.map(tag -> ("'" + SqlUtil.doubleQuotes(tag) + "'")).collect(Collectors.joining(","));
				if (StringUtils.isNotEmpty(currentlyUsedTagsStr))
					jdbcUpdate("delete from ld_uniquetag where ld_tenantid=" + tenantId + " and ld_tag not in ("
							+ currentlyUsedTagsStr + ")");
			}
		}
	}

	@Override
	public void insertNewUniqueTags() throws PersistenceException {
		StringBuilder insertStatement = new StringBuilder("insert into ld_uniquetag(ld_tag, ld_tenantid, ld_count) ");
		insertStatement.append(" select distinct(B.ld_tag), B.ld_tenantid, 0 from ld_tag B, ld_document D ");
		insertStatement.append(
				" where B.ld_docid = D.ld_id and D.ld_deleted = 0 and B.ld_tag not in (select A.ld_tag from ld_uniquetag A where A.ld_tenantid=B.ld_tenantid) ");
		jdbcUpdate(insertStatement.toString());

		insertStatement = new StringBuilder("insert into ld_uniquetag(ld_tag, ld_tenantid, ld_count) ");
		insertStatement.append(" select distinct(B.ld_tag), B.ld_tenantid, 0 from ld_foldertag B, ld_folder F ");
		insertStatement.append(
				" where B.ld_folderid = F.ld_id and F.ld_deleted = 0 and B.ld_tag not in (select A.ld_tag from ld_uniquetag A where A.ld_tenantid=B.ld_tenantid) ");
		jdbcUpdate(insertStatement.toString());
	}

	@Override
	public void updateCountUniqueTags() throws PersistenceException {
		List<Long> tenantIds = tenantDAO.findAllIds();
		for (Long tenantId : tenantIds) {
			// Get the actual unique tags for the given tenant
			List<String> uniqueTags = queryForList("select ld_tag from ld_uniquetag", String.class);

			// Update the count for each tag skipping those tags that belong to
			// deleted documents
			for (String tag : uniqueTags) {
				try {
					Map<String, Object> params = new HashMap<>();
					params.put("tag", tag);
					params.put("tenantId", tenantId);

					jdbcUpdate(
							"update ld_uniquetag set ld_count = (select count(T.ld_tag) from ld_tag T, ld_document D where T.ld_tag = :tag and T.ld_tenantid = :tenantId "
									+ " and T.ld_docid = D.ld_id and D.ld_deleted=0 ) where ld_tag = :tag and ld_tenantid = :tenantId",
							params);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			}
		}
	}

	@Override
	public List<TagCloud> getTagCloud(long tenantId, int maxTags) throws PersistenceException {
		GenericDAO gendao = GenericDAO.get();

		List<TagCloud> list = gendao.query(
				"select ld_tag, ld_count from ld_uniquetag where ld_tenantid=" + tenantId + " order by ld_count desc",
				new RowMapper<TagCloud>() {

					@Override
					public TagCloud mapRow(ResultSet rs, int arg1) throws SQLException {
						return new TagCloud(rs.getString(1), rs.getLong(2));
					}
				}, null);

		/**
		 * Get the most used tags
		 */
		List<TagCloud> mostUsedTags = list;
		if (maxTags > 0 && mostUsedTags.size() > maxTags)
			mostUsedTags = new ArrayList<>(list.subList(0, maxTags));

		if (mostUsedTags != null && !mostUsedTags.isEmpty()) {
			// Find the Max frequency
			long maxValue = mostUsedTags.get(0).getCount();

			for (TagCloud cloud : mostUsedTags) {
				double scale = ((double) cloud.getCount()) / maxValue;
				int scaleInt = (int) Math.ceil(scale * 10);
				cloud.setScale(scaleInt);
			}

			// Sort the tags collection by name
			Collections.sort(mostUsedTags);
		}

		return mostUsedTags;
	}

	@Override
	public List<TagCloud> getTagCloud(String sid) throws PersistenceException {
		Session session = SessionManager.get().get(sid);
		int maxTags = Context.get().getProperties().getInt(session.getTenantName() + ".tagcloud.maxtags", 30);
		return getTagCloud(session.getTenantId(), maxTags);
	}

	@Override
	public Document findDocument(long docId) throws PersistenceException {
		Document doc = findById(docId);
		if (doc != null && doc.getDocRef() != null)
			doc = findById(doc.getDocRef());
		return doc;
	}

	@Override
	public Folder getWorkspace(long docId) throws PersistenceException {
		Document doc = findById(docId);
		if (doc == null)
			return null;

		return folderDAO.findWorkspace(doc.getFolder().getId());
	}

	@Override
	public void setPassword(long docId, String password, DocumentHistory transaction) throws PersistenceException {
		if (transaction == null)
			throw new IllegalArgumentException(TRANSACTION_CANNOT_BE_NULL);
		if (transaction.getUsername() == null)
			throw new IllegalArgumentException("transaction username cannot be null");

		transaction.setEvent(DocumentEvent.PASSWORD_PROTECTED);

		Document doc = findDocument(docId);
		if (doc != null) {
			if (StringUtils.isNotEmpty(doc.getPassword()))
				throw new PersistenceException("The document already has a password, unset it first");

			initialize(doc);
			try {
				doc.setDecodedPassword(password);
			} catch (NoSuchAlgorithmException e) {
				throw new PersistenceException("Cannot cript the password", e);
			}
			store(doc, transaction);

			List<Version> versions = versionDAO.findByDocId(docId);
			for (Version ver : versions) {
				versionDAO.initialize(ver);
				ver.setPassword(doc.getPassword());
				versionDAO.store(ver);
			}
		}
	}

	@Override
	public void unsetPassword(long docId, DocumentHistory transaction) throws PersistenceException {
		if (transaction == null)
			throw new IllegalArgumentException(TRANSACTION_CANNOT_BE_NULL);
		if (transaction.getUsername() == null)
			throw new IllegalArgumentException("transaction username cannot be null");

		transaction.setEvent(DocumentEvent.PASSWORD_UNPROTECTED);

		Document doc = findDocument(docId);
		if (doc != null) {
			initialize(doc);
			doc.setPassword(null);
			store(doc, transaction);

			List<Version> versions = versionDAO.findByDocId(docId);
			for (Version ver : versions) {
				versionDAO.initialize(ver);
				ver.setPassword(null);
				versionDAO.store(ver);
			}
		}
	}

	@Override
	public Document findByPath(String path, long tenantId) throws PersistenceException {
		String folderPath = FileUtil.getPath(path);
		Folder folder = folderDAO.findByPathExtended(folderPath, tenantId);
		if (folder == null)
			return null;

		String fileName = FileUtil.getName(path);
		List<Document> docs = findByFileNameAndParentFolderId(folder.getId(), fileName, null, tenantId, null);
		for (Document doc : docs) {
			if (doc.getFileName().equals(fileName))
				return doc;
		}

		return null;
	}

	@Override
	public List<String> findDuplicatedDigests(Long tenantId, Long folderId) throws PersistenceException {
		// First of all, find all duplicates digests.
		StringBuilder digestQuery = new StringBuilder("select ld_digest from ld_document where ld_deleted = 0 ");
		if (tenantId != null) {
			digestQuery.append(" and ld_tenantid = ");
			digestQuery.append(Long.toString(tenantId));
		}

		if (folderId != null) {
			List<Long> tree = folderDAO.findIdsByParentId(folderId);
			if (!tree.contains(folderId))
				tree.add(folderId);
			digestQuery.append(" and ld_folderid in (");
			digestQuery.append(tree.stream().map(Object::toString).collect(Collectors.joining(", ")));
			digestQuery.append(" ) ");
		}
		digestQuery.append(" and ld_docref is null and ld_digest is not null group by ld_digest having count(*) > 1");

		return query(digestQuery.toString(), (rs, rowNum) -> rs.getString(1), null);

	}

	@Override
	public boolean isPrintAllowed(long id, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.PRINT, id, userId);
	}

	@Override
	public boolean isWriteAllowed(long id, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.WRITE, id, userId);
	}

	@Override
	public boolean isDownloadAllowed(long id, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.DOWNLOAD, id, userId);
	}

	@Override
	public boolean isMoveAllowed(long id, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.MOVE, id, userId);
	}

	@Override
	public boolean isReadAllowed(long docId, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.READ, docId, userId);
	}

	@Override
	public boolean isPreviewAllowed(long docId, long userId) throws PersistenceException {
		return isPermissionAllowed(Permission.PREVIEW, docId, userId);
	}

	@Override
	public boolean isPermissionAllowed(Permission permission, long documentId, long userId)
			throws PersistenceException {
		Set<Permission> permissions = getAllowedPermissions(documentId, userId);
		return permissions.contains(permission);
	}

	private User getExistingtUser(long userId) throws PersistenceException {
		User user = userDAO.findById(userId);
		if (user == null)
			throw new PersistenceException("Unexisting user " + userId);
		return user;
	}

	@Override
	public Set<Permission> getAllowedPermissions(long docId, long userId) throws PersistenceException {
		final Set<Permission> permissions = new HashSet<>();
		User user = getExistingtUser(userId);

		userDAO.initialize(user);

		if (findById(docId) == null)
			return new HashSet<>();

		// If the user is an administrator bypass all controls
		if (user.isAdmin()) {
			return Permission.all();
		}

		StringBuilder query = new StringBuilder("""
						select ld_read as LDREAD, ld_write as LDWRITE, ld_security as LDSECURITY, ld_immutable as LDIMMUTABLE, ld_delete as LDDELETE,
						ld_rename as LDRENAME, ld_sign as LDSIGN, ld_archive as LDARCHIVE, ld_workflow as LDWORKFLOW, ld_download as LDDOWNLOAD,
						ld_calendar as LDCALENDAR, ld_subscription as LDSUBSCRIPTION, ld_print as LDPRINT, ld_password as LDPASSWORD,
						ld_move as LDMOVE, ld_email as LDEMAIL, ld_automation as LDAUTOMATION, ld_readingreq as LDREADINGREQ, ld_preview as LDPREVIEW,
						ld_customid as LDCUSTOMID, ld_revision as LDREVISION from ld_document_acl where ld_docid=
						""");
		query.append(Long.toString(docId));
		query.append(" and ld_groupid in (select ld_groupid from ld_usergroup where ld_userid=");
		query.append(Long.toString(userId));
		query.append(")");

		final Map<String, Permission> permissionColumn = new HashMap<>();
		permissionColumn.put("LDDELETE", Permission.DELETE);
		permissionColumn.put("LDIMMUTABLE", Permission.IMMUTABLE);
		permissionColumn.put("LDSECURITY", Permission.SECURITY);
		permissionColumn.put("LDRENAME", Permission.RENAME);
		permissionColumn.put("LDWRITE", Permission.WRITE);
		permissionColumn.put("LDREAD", Permission.READ);
		permissionColumn.put("LDSIGN", Permission.SIGN);
		permissionColumn.put("LDARCHIVE", Permission.ARCHIVE);
		permissionColumn.put("LDWORKFLOW", Permission.WORKFLOW);
		permissionColumn.put("LDDOWNLOAD", Permission.DOWNLOAD);
		permissionColumn.put("LDCALENDAR", Permission.CALENDAR);
		permissionColumn.put("LDSUBSCRIPTION", Permission.SUBSCRIPTION);
		permissionColumn.put("LDPRINT", Permission.PRINT);
		permissionColumn.put("LDPASSWORD", Permission.PASSWORD);
		permissionColumn.put("LDMOVE", Permission.MOVE);
		permissionColumn.put("LDEMAIL", Permission.EMAIL);
		permissionColumn.put("LDAUTOMATION", Permission.AUTOMATION);
		permissionColumn.put("LDREADINGREQ", Permission.READINGREQ);
		permissionColumn.put("LDPREVIEW", Permission.PREVIEW);
		permissionColumn.put("LDCUSTOMID", Permission.CUSTOMID);
		permissionColumn.put("LDREVISION", Permission.REVISION);

		queryForResultSet(query.toString(), null, null, rows -> {
			while (rows.next()) {
				for (Entry<String, Permission> entry : permissionColumn.entrySet()) {
					String column = entry.getKey();
					Permission permission = entry.getValue();
					if (rows.getInt(column) == 1)
						permissions.add(permission);
				}
			}
		});

		if (permissions.isEmpty()) {
			// The document does not specify its own permissions so use the
			// folder's ones
			long folderId = queryForLong("select ld_folderid from ld_document where ld_id = " + docId);
			return folderDAO.getAllowedPermissions(folderId, userId);
		} else {
			return permissions;
		}
	}

	@Override
	public void applyParentFolderSecurity(long docId, DocumentHistory transaction) throws PersistenceException {

		// Get the folder that owns the security policies
		Folder folder = folderDAO.findById(queryForLong("select ld_folderid from ld_document where ld_id=" + docId));
		while (folder.getSecurityRef() != null)
			folder = folderDAO.findById(folder.getSecurityRef());

		int count = jdbcUpdate("delete from ld_document_acl where ld_docid=" + docId);
		log.debug("Removed {} security policies of document {}", count, docId);

		StringBuilder update = new StringBuilder("""
				 insert into ld_document_acl(ld_docId,ld_groupid,ld_read,ld_preview,ld_write,ld_security,
				                              ld_immutable,ld_delete,ld_rename,ld_sign,ld_archive,
				                              ld_workflow,ld_download,ld_calendar,ld_subscription,
				                              ld_print,ld_password,ld_move,ld_email,ld_automation,
				                              ld_readingreq, ld_customid, ld_revision)
				""");
		update.append(" select ");
		update.append(Long.toString(docId));
		update.append("""
				,ld_groupid,ld_read,ld_preview,ld_write,ld_security,
				                              ld_immutable,ld_delete,ld_rename,ld_sign,ld_archive,
				                              ld_workflow,ld_download,ld_calendar,ld_subscription,
				                              ld_print,ld_password,ld_move,ld_email,ld_automation,
				                              ld_readingreq, ld_customid, ld_revision from ld_folder_acl where ld_folderid=
				""");
		update.append(Long.toString(folder.getId()));
		count = jdbcUpdate(update.toString());
		log.debug("Copied {} security policies of folder {} into document {}", count, folder.getId(), docId);

		if (transaction != null) {
			transaction.setEvent(DocumentEvent.PERMISSION);
			Document document = findById(docId);
			saveDocumentHistory(document, transaction);
		}
	}
}