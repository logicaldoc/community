package com.logicaldoc.core.document.dao;

import java.io.IOException;
import java.io.InputStream;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.communication.EventCollector;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentLink;
import com.logicaldoc.core.document.DocumentListener;
import com.logicaldoc.core.document.DocumentListenerManager;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.Tag;
import com.logicaldoc.core.document.TagCloud;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.sql.SqlUtil;

/**
 * Hibernate implementation of <code>DocumentDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateDocumentDAO extends HibernatePersistentObjectDAO<Document> implements DocumentDAO {
	private DocumentHistoryDAO documentHistoryDAO;

	private VersionDAO versionDAO;

	private TenantDAO tenantDAO;

	private DocumentNoteDAO noteDAO;

	private FolderDAO folderDAO;

	private UserDAO userDAO;

	private DocumentLinkDAO linkDAO;

	private DocumentListenerManager listenerManager;

	private Storer storer;

	private ContextProperties config;

	private HibernateDocumentDAO() {
		super(Document.class);
		super.log = LoggerFactory.getLogger(HibernateDocumentDAO.class);
	}

	public void setListenerManager(DocumentListenerManager listenerManager) {
		this.listenerManager = listenerManager;
	}

	public void setVersionDAO(VersionDAO versionDAO) {
		this.versionDAO = versionDAO;
	}

	public void setLinkDAO(DocumentLinkDAO linkDAO) {
		this.linkDAO = linkDAO;
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	public void setDocumentHistoryDAO(DocumentHistoryDAO documentHistoryDAO) {
		this.documentHistoryDAO = documentHistoryDAO;
	}

	@Override
	public boolean archive(long docId, DocumentHistory transaction) {
		boolean result = true;
		try {
			Document doc = (Document) findById(docId);
			doc.setStatus(AbstractDocument.DOC_ARCHIVED);
			if (doc.getIndexed() != AbstractDocument.INDEX_SKIP)
				doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			doc.setLockUserId(transaction.getUserId());
			transaction.setEvent(DocumentEvent.ARCHIVED.toString());
			store(doc, transaction);
			log.debug("Archived document {}", docId);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public void unarchive(long docId, DocumentHistory transaction) {
		try {
			Document doc = (Document) findById(docId);
			doc.setStatus(AbstractDocument.DOC_UNLOCKED);
			doc.setLockUserId(null);
			transaction.setEvent(DocumentEvent.RESTORED.toString());
			store(doc, transaction);
			log.debug("Unarchived document {}", docId);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public boolean delete(long docId, DocumentHistory transaction) {
		return delete(docId, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public boolean delete(long docId, int delCode, DocumentHistory transaction) {
		assert (delCode != 0);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		if (!checkStoringAspect())
			return false;

		boolean result = true;
		try {
			Document doc = (Document) findById(docId);
			if (doc != null && doc.getImmutable() == 0
					|| (doc != null && doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin"))) {

				// Remove versions
				try {
					for (Version version : versionDAO.findByDocId(docId)) {
						version.setDeleted(delCode);
						saveOrUpdate(version);
					}
				} catch (Throwable t) {
					log.error("Error removing the versions of document {}", doc, t);
				}

				// Remove notes
				try {
					for (DocumentNote note : noteDAO.findByDocId(docId, null)) {
						note.setDeleted(delCode);
						saveOrUpdate(note);
					}
				} catch (Throwable t) {
					log.error("Error removing the notes of document {}", doc, t);
				}

				// Remove links
				try {
					for (DocumentLink link : linkDAO.findByDocId(docId)) {
						link.setDeleted(delCode);
						saveOrUpdate(link);
					}
				} catch (Throwable t) {
					log.error("Error removing the links of document {}", doc, t);
				}

				doc.setDeleted(delCode);
				doc.setDeleteUserId(transaction.getUserId());

				if (transaction.getUser() != null)
					doc.setDeleteUser(transaction.getUser().getFullName());
				else
					doc.setDeleteUser(transaction.getUsername());

				if (doc.getCustomId() != null)
					doc.setCustomId(doc.getCustomId() + "." + doc.getId());
				result = store(doc, transaction);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}

		return result;
	}

	@Override
	public List<Long> findByUserId(long userId) {
		Collection<Folder> folders = folderDAO.findByUserId(userId);
		if (folders.isEmpty())
			return new ArrayList<Long>();

		StringBuffer query = new StringBuffer();
		query.append(ALIAS_ENTITY+".folder.id in (");
		boolean first = true;
		for (Folder folder : folders) {
			if (!first)
				query.append(",");
			query.append(folder.getId());
			first = false;
		}
		query.append(") and not "+ALIAS_ENTITY+".status=" + AbstractDocument.DOC_ARCHIVED);
		try {
			return findIdsByWhere(query.toString(), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public List<Document> findByLockUserAndStatus(Long userId, Integer status) {
		StringBuffer sb = new StringBuffer(
				"select ld_id, ld_folderid, ld_version, ld_fileversion, ld_lastmodified, ld_filename from ld_document where ld_deleted = 0 ");
		if (userId != null)
			sb.append(" and ld_lockuserid=" + userId);

		if (status != null)
			sb.append(" and ld_status=" + status);

		try {
			return (List<Document>) query(sb.toString(), null, new RowMapper<Document>() {

				@Override
				public Document mapRow(ResultSet rs, int col) throws SQLException {
					Document doc = new Document();
					doc.setId(rs.getLong(1));
					Folder folder = new Folder();
					folder.setId(rs.getLong(2));
					doc.setFolder(folder);
					doc.setVersion(rs.getString(3));
					doc.setFileVersion(rs.getString(4));
					doc.setLastModified(rs.getTimestamp(5));
					doc.setFileName(rs.getString(6));
					return doc;
				}

			}, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Document>();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Long> findDocIdByTag(String tag) {
		StringBuilder query = new StringBuilder(
				"select distinct(A.ld_docid) from ld_tag A, ld_document B where A.ld_docid=B.ld_id and not B.ld_status="
						+ AbstractDocument.DOC_ARCHIVED);
		query.append(" and lower(ld_tag)='" + SqlUtil.doubleQuotes(tag).toLowerCase() + "'");
		try {
			return (List<Long>) queryForList(query.toString(), Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public boolean store(final Document doc) throws PersistenceException {
		return store(doc, null);
	}

	@Override
	public boolean store(Document doc, final DocumentHistory transaction) throws PersistenceException {
		if (!checkStoringAspect())
			return false;

		boolean result = true;
		try {
			Tenant tenant = tenantDAO.findById(doc.getTenantId());

			if (transaction != null) {
				transaction.setTenantId(doc.getTenantId());
				transaction.setTenant(tenant.getName());
			}

			// Truncate publishing dates
			if (doc.getStartPublishing() != null)
				doc.setStartPublishing(DateUtils.truncate(doc.getStartPublishing(), Calendar.DATE));
			if (doc.getStopPublishing() != null)
				doc.setStopPublishing(DateUtils.truncate(doc.getStopPublishing(), Calendar.DATE));

			if (doc.getIndexed() == Document.INDEX_TO_INDEX || doc.getIndexed() == Document.INDEX_TO_INDEX_METADATA) {
				// Check if the document must be indexed
				if (!FileUtil.matches(doc.getFileName(), config.getProperty(tenant.getName() + ".index.includes", ""),
						config.getProperty(tenant.getName() + ".index.excludes", "")))
					doc.setIndexed(Document.INDEX_SKIP);

				// Check if the document must be indexed
				if (doc.getIndexed() == Document.INDEX_SKIP && FileUtil.matches(doc.getFileName(),
						config.getProperty(tenant.getName() + ".index.includes.metadata", ""),
						config.getProperty(tenant.getName() + ".index.excludes.metadata", "")))
					doc.setIndexed(Document.INDEX_TO_INDEX_METADATA);
			}

			Set<Tag> src = doc.getTags();
			if (src != null && src.size() > 0) {
				// Trim too long tags
				Set<Tag> dst = new HashSet<Tag>();
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

			/*
			 * Avoid documents inside folder alias
			 */
			if (doc.getFolder().getFoldRef() != null) {
				Folder fld = folderDAO.findById(doc.getFolder().getFoldRef());
				if (fld == null)
					throw new Exception(
							String.format("Unable to find refrenced folder %s", doc.getFolder().getFoldRef()));
				doc.setFolder(fld);
			}

			if (doc.getDocRef() == null) {
				/*
				 * In case of a regular document, check for attributes defaults
				 * specified at folder's level
				 */
				if (doc.getFolder().getTemplate() != null) {
					folderDAO.initialize(doc.getFolder());
					if (doc.getTemplate() == null || doc.getTemplate().equals(doc.getFolder().getTemplate())) {
						doc.setTemplate(doc.getFolder().getTemplate());
						try {
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
						} catch (Throwable t) {
							// From times to times during check-in a
							// lazy-loading
							// exception is thrown
						}
					}
				}

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

			if (!RunLevel.current().aspectEnabled("customId"))
				doc.setCustomId(UUID.randomUUID().toString());

			log.debug("Invoke listeners before store");
			Map<String, Object> dictionary = new HashMap<String, Object>();
			for (DocumentListener listener : listenerManager.getListeners())
				listener.beforeStore(doc, transaction, dictionary);

			if (StringUtils.isEmpty(doc.getCustomId()))
				doc.setCustomId(UUID.randomUUID().toString());

			// Use unique filename in the same folder
			setUniqueFilename(doc);

			// Save the document
			saveOrUpdate(doc);
			try {
				flush();
			} catch (Throwable t) {
				// Noting to do
			}
			if (doc.getDeleted() == 0 && doc.getId() != 0L)
				refresh(doc);

			doc.setModified(false);

			log.debug("Invoke listeners after store");
			for (DocumentListener listener : listenerManager.getListeners())
				listener.afterStore(doc, transaction, dictionary);

			if (StringUtils.isEmpty(doc.getCustomId())) {
				doc.setCustomId(Long.toString(doc.getId()));
				doc.setModified(true);
			}

			// Perhaps some listeners may have modified the document
			if (doc.isModified())
				saveOrUpdate(doc);
			saveDocumentHistory(doc, transaction, dictionary);

			/**
			 * Update the aliases
			 */
			if (doc.getDocRef() == null)
				jdbcUpdate("update ld_document set ld_filesize= " + doc.getFileSize() + ", ld_pages= " + doc.getPages()
						+ ", ld_version='" + doc.getVersion() + "', ld_fileversion='" + doc.getFileVersion()
						+ "' where ld_docref= " + doc.getId());
		} catch (Throwable e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			result = false;
			if (e instanceof PersistenceException)
				throw (PersistenceException) e;
			else
				throw new PersistenceException(e);
		}

		return result;
	}

	/**
	 * Avoid file name duplications in the same folder
	 */
	private void setUniqueFilename(Document doc) {
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
		final Set<String> fileNames = new HashSet<String>();

		StringBuffer query = new StringBuffer(
				"select lower(ld_filename) from ld_document where ld_deleted=0 and ld_folderid=");
		query.append(Long.toString(doc.getFolder().getId()));
		query.append(" and lower(ld_filename) like '");
		query.append(SqlUtil.doubleQuotes(baseName.toLowerCase()));
		query.append("%' and not ld_id=");
		query.append(Long.toString(doc.getId()));

		// Execute the query to populate the sets
		try {
			SqlRowSet rs = queryForRowSet(query.toString(), null, null);
			if (rs != null)
				while (rs.next()) {
					String file = rs.getString(1);
					if (!fileNames.contains(file))
						fileNames.add(file);
				}
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		int counter = 1;
		while (fileNames.contains(doc.getFileName().toLowerCase()))
			doc.setFileName(baseName + "(" + (counter++) + ")" + ext);
	}

	@Override
	public void updateDigest(Document doc) throws PersistenceException {
		String resource = storer.getResourceName(doc, doc.getFileVersion(), null);
		if (storer.exists(doc.getId(), resource)) {
			
			try(InputStream in = storer.getStream(doc.getId(), resource);) {
				doc.setDigest(FileUtil.computeDigest(in));
			} catch (IOException e) {
				log.error("Cannot retrieve the conten of document {}", doc, e);
			}
			
			jdbcUpdate("update ld_document set ld_lastmodified=?, ld_digest=?  where ld_id=?", new Date(),
					doc.getDigest(), doc.getId());

			// Update the versions also
			jdbcUpdate("update ld_version set ld_digest=?  where ld_documentid=? and ld_fileversion=?", doc.getDigest(),
					doc.getId(), doc.getFileVersion());
		}
	}

	@SuppressWarnings("unchecked")
	public List<Document> findLastModifiedByUserId(long userId, int maxElements) throws PersistenceException {
		List<Document> coll = new ArrayList<Document>();

		StringBuilder query = new StringBuilder("SELECT _history.docId from DocumentHistory _history");
		query.append(" WHERE _history.userId = " + Long.toString(userId) + " ");
		query.append(" ORDER BY _history.date DESC");

		List<Long> results = new ArrayList<Long>();
		try {
			results = (List<Long>) findByQuery(query.toString(), (Map<String, Object>) null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		for (Long docid : results) {
			if (coll.size() >= maxElements)
				break;
			if (docid != null) {
				Document document = findById(docid);
				if (folderDAO.isReadEnabled(document.getFolder().getId(), userId))
					coll.add(document);
			}
		}

		return coll;
	}

	@SuppressWarnings("unchecked")
	public List<String> findTags(long docId) {
		try {
			return queryForList("select ld_tag from ld_tag where ld_docid=" + docId + " order by ld_tag", String.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<String>();
		}
	}

	@Override
	public Map<String, Long> findTags(String firstLetter, Long tenantId) {
		final Map<String, Long> map = new HashMap<String, Long>();

		try {
			StringBuilder query = new StringBuilder("SELECT ld_count, ld_tag from ld_uniquetag where 1=1 ");
			if (StringUtils.isNotEmpty(firstLetter))
				query.append(" and lower(ld_tag) like '" + firstLetter.toLowerCase() + "%' ");
			if (tenantId != null)
				query.append(" and ld_tenantid=" + tenantId);

			query(query.toString(), null, new RowMapper<Object>() {

				@Override
				public Object mapRow(ResultSet rs, int rowNumber) throws SQLException {
					Long value = (Long) rs.getLong(1);
					String key = (String) rs.getString(2);
					map.put(key, value);
					return null;
				}

			}, null);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return map;
	}

	@Override
	public List<String> findAllTags(String firstLetter, Long tenantId) {
		try {
			StringBuilder sb = new StringBuilder("select ld_tag from ld_uniquetag where 1=1 ");
			if (tenantId != null) {
				sb.append(" and ld_tenantid=" + tenantId);
			}

			List<Object> parameters = new ArrayList<Object>();
			if (firstLetter != null) {
				sb.append(" and lower(ld_tag) like ? ");
				parameters.add(firstLetter.toLowerCase() + "%");
			}
			return (List<String>) queryForList(sb.toString(), parameters.toArray(new Object[0]), String.class, null);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return new ArrayList<String>();
	}

	@Override
	public List<Document> findByUserIdAndTag(long userId, String tag, Integer max) {
		List<Document> coll = new ArrayList<Document>();

		List<Long> ids = findDocIdByUserIdAndTag(userId, tag);
		if (!ids.isEmpty()) {
			StringBuffer query = new StringBuffer("select A from Document A where A.id in (");
			query.append(ids.stream().map(f -> f.toString()).collect(Collectors.joining(",")));
			query.append(")");
			try {
				coll = (List<Document>) findByQuery(query.toString(), (Map<String, Object>) null, max);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return coll;
	}

	@Override
	public List<Long> findDocIdByUserIdAndTag(long userId, String tag) {
		List<Long> ids = new ArrayList<Long>();
		try {
			User user = userDAO.findById(userId);
			if (user == null)
				return ids;

			StringBuffer query = new StringBuffer();

			if (user.isMemberOf("admin")) {
				ids = findDocIdByTag(tag);
			} else {

				/*
				 * Search for all accessible folders
				 */
				Collection<Long> precoll = folderDAO.findFolderIdByUserId(userId, null, true);
				String precollString = precoll.toString().replace('[', '(').replace(']', ')');

				query.append("select distinct(C.ld_id) from ld_document C, ld_tag D "
						+ " where C.ld_id=D.ld_docid AND C.ld_deleted=0 and not C.ld_status="
						+ AbstractDocument.DOC_ARCHIVED);
				query.append(" AND C.ld_folderid in ");
				query.append(precollString);
				query.append(" AND lower(D.ld_tag)='" + SqlUtil.doubleQuotes(tag.toLowerCase()) + "' ");

				log.debug("Find by tag: {}", query);

				List<Long> docIds = (List<Long>) queryForList(query.toString(), Long.class);
				ids.addAll(docIds);
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return ids;
	}

	@Override
	public List<Document> findLastDownloadsByUserId(long userId, int maxResults) {
		List<Document> coll = new ArrayList<Document>();

		try {
			StringBuffer query = new StringBuffer("select docId from DocumentHistory ");
			query.append(" where userId = " + userId);
			query.append(" and event = '" + DocumentEvent.DOWNLOADED + "' ");
			query.append(" order by date desc");

			List<Long> results = (List<Long>) findByQuery(query.toString(), (Map<String, Object>) null, null);
			ArrayList<Long> tmpal = new ArrayList<Long>(results);
			List<Long> docIds = tmpal;

			if (docIds.isEmpty())
				return coll;

			if (docIds.size() > maxResults) {
				tmpal.subList(0, maxResults - 1);
			}

			query = new StringBuffer("from Document "+ALIAS_ENTITY+" ");
			query.append(" where not "+ALIAS_ENTITY+".status=" + AbstractDocument.DOC_ARCHIVED);
			query.append(" and "+ALIAS_ENTITY+".id in (");

			for (int i = 0; i < docIds.size(); i++) {
				Long docId = docIds.get(i);
				if (i > 0)
					query.append(",");
				query.append(docId);
			}
			query.append(")");

			// execute the query
			List<Document> unorderdColl = (List<Document>) findByQuery(query.toString(), (Map<String, Object>) null,
					null);

			// put all elements in a map
			HashMap<Long, Document> hm = new HashMap<Long, Document>();
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
		} catch (Exception e) {
			log.warn(e.getMessage(), e);
		}

		return coll;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Long> findDocIdByFolder(long folderId, Integer max) {
		String sql = "select ld_id from ld_document where ld_deleted=0 and ld_folderid = " + folderId
				+ " and not ld_status=" + AbstractDocument.DOC_ARCHIVED;
		try {
			return (List<Long>) queryForList(sql, null, Long.class, max);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public List<Document> findByFolder(long folderId, Integer max) {
		try {
			Map<String, Object> params = new HashMap<String, Object>();
			params.put("folderId", Long.valueOf(folderId));
			return findByWhere(ALIAS_ENTITY+".folder.id = :folderId ", params, null, max);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
			return new ArrayList<Document>();
		}
	}

	@Override
	public List<Document> findArchivedByFolder(long folderId) {
		try {
			return findByWhere(
					ALIAS_ENTITY+".folder.id = " + folderId + " and "+ALIAS_ENTITY+".status=" + AbstractDocument.DOC_ARCHIVED, null,
					null);
		} catch (PersistenceException e) {
			log.warn(e.getMessage(), e);
			return new ArrayList<Document>();
		}
	}

	@Override
	public List<Document> findLinkedDocuments(long docId, String linkType, Integer direction) {
		List<Document> coll = new ArrayList<Document>();
		StringBuffer query = null;
		try {
			query = new StringBuffer("");
			if (direction == null)
				query.append(
						"select distinct(ld_docid2) from ld_link where ld_deleted=0 and (ld_docid1=?) UNION select distinct(ld_docid1) from ld_link where ld_deleted=0 and (ld_docid2=?)");
			else if (direction.intValue() == 1)
				query.append("select distinct(ld_docid2) from ld_link where ld_deleted=0 and (ld_docid1=?)");
			else if (direction.intValue() == 2)
				query.append("select distinct(ld_docid1) from ld_link where ld_deleted=0 and (ld_docid2=?)");
			@SuppressWarnings("unchecked")
			List<Long> ids = (List<Long>) queryForList(query.toString(),
					linkType != null ? new Object[] { docId } : new Object[] { docId, docId }, Long.class, null);
			coll = findByWhere(
					ALIAS_ENTITY+".id in (" + ids.stream().map(id -> id.toString()).collect(Collectors.joining(","))
							+ ") and not "+ALIAS_ENTITY+".status=" + AbstractDocument.DOC_ARCHIVED,
					null, null);

		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
		}

		return coll;
	}

	@Override
	public List<Document> findByFileNameAndParentFolderId(Long folderId, String fileName, Long excludeId, Long tenantId,
			Integer max) {
		String query = "lower("+ALIAS_ENTITY+".fileName) like '%" + SqlUtil.doubleQuotes(fileName.toLowerCase()) + "%'";
		if (tenantId != null) {
			query += " and "+ALIAS_ENTITY+".tenantId = " + tenantId;
		}
		if (folderId != null) {
			query += " and "+ALIAS_ENTITY+".folder.id = " + folderId;
		}
		if (excludeId != null)
			query += " and not("+ALIAS_ENTITY+".id = " + excludeId + ")";
		query += " and not "+ALIAS_ENTITY+".status=" + AbstractDocument.DOC_ARCHIVED;

		try {
			return findByWhere(query, null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Document>();
		}
	}

	@Override
	public void initialize(Document doc) {
		try {
			refresh(doc);

			if (doc.getAttributes() != null)
				log.trace("Initialized {} attributes", doc.getAttributes().keySet().size());

			if (doc.getTags() != null)
				log.trace("Initialized {} tags", doc.getTags().size());
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Long> findDeletedDocIds() {
		String query = "select ld_id from ld_document where ld_deleted=1 order by ld_lastmodified desc";
		try {
			return (List<Long>) queryForList(query, Long.class);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public List<Document> findDeletedDocs() {
		List<Document> coll = new ArrayList<Document>();
		try {
			String query = "select ld_id, ld_customid, ld_lastModified, ld_filename from ld_document where ld_deleted=1 order by ld_lastmodified desc";

			@SuppressWarnings("rawtypes")
			RowMapper docMapper = new BeanPropertyRowMapper() {
				public Object mapRow(ResultSet rs, int rowNum) throws SQLException {

					Document doc = new Document();
					doc.setId(rs.getLong(1));
					doc.setCustomId(rs.getString(2));
					doc.setLastModified(rs.getDate(3));
					doc.setFileName(rs.getString(4));

					return doc;
				}
			};

			coll = (List<Document>) query(query, new Object[] {}, docMapper, null);
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
		}
		return coll;
	}

	@Override
	public long computeTotalSize(Long tenantId, Long userId, boolean computeDeleted) {
		long sizeDocs = 0;

		try {
			// we do not count the aliases
			sizeDocs = queryForLong("SELECT SUM(ld_filesize) from ld_document where ld_docref is null "
					+ (computeDeleted ? "" : " and ld_deleted=0 ")
					+ (userId != null ? " and ld_publisherid=" + userId : "")
					+ (tenantId != Tenant.SYSTEM_ID ? " and ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		long sizeVersions = 0;

		try {
			sizeVersions = queryForLong(
					"select SUM(V.ld_filesize) from ld_version V where V.ld_version = V.ld_fileversion"
							+ (computeDeleted ? "" : " and V.ld_deleted=0 ")
							+ (userId != null ? " and V.ld_publisherid=" + userId : "")
							+ (tenantId != Tenant.SYSTEM_ID ? " and V.ld_tenantid=" + tenantId : "")
							+ "   and not exists (select D.ld_id from ld_document D"
							+ "                   where D.ld_id=V.ld_documentid "
							+ "                     and D.ld_fileversion=V.ld_fileversion)");
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		return sizeDocs + sizeVersions;
	}

	@Override
	public long count(Long tenantId, boolean computeDeleted, boolean computeArchived) {
		String query = "select count(*) from ld_document where 1=1 ";
		if (!computeDeleted)
			query += " and ld_deleted = 0 ";
		if (!computeArchived)
			query += " and not ld_status = " + AbstractDocument.DOC_ARCHIVED;
		if (tenantId != null)
			query += " and ld_tenantid = " + tenantId;

		try {
			return queryForLong(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Document> findByIndexed(int indexed) {
		try {
			return findByWhere(ALIAS_ENTITY+".docRef is null and "+ALIAS_ENTITY+".indexed=" + indexed,
					"order by "+ALIAS_ENTITY+".lastModified asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Document>();
		}
	}

	@Override
	public void restore(long docId, long folderId, final DocumentHistory transaction) throws PersistenceException {
		bulkUpdate("set ld_deleted=0, ld_folderid=" + folderId + ", ld_lastmodified=CURRENT_TIMESTAMP where ld_id="
				+ docId, (Map<String, Object>) null);

		versionDAO.bulkUpdate("set ld_deleted=0, ld_folderid=" + folderId
				+ ", ld_lastmodified=CURRENT_TIMESTAMP where ld_documentid=" + docId, (Map<String, Object>) null);

		Document doc = findById(docId);
		if (doc != null && transaction != null) {
			transaction.setDocId(docId);
			transaction.setEvent(DocumentEvent.RESTORED.toString());

			initialize(doc);
			store(doc, transaction);
		}
	}

	@Override
	public Document findByCustomId(String customId, long tenantId) {
		Document doc = null;
		if (customId != null)
			try {
				String query = ALIAS_ENTITY+".customId = '" + SqlUtil.doubleQuotes(customId) + "' " + " and "+ALIAS_ENTITY+".tenantId="
						+ tenantId;
				List<Document> coll = findByWhere(query, null, null);
				if (!coll.isEmpty()) {
					doc = coll.get(0);
					if (doc.getDeleted() == 1)
						doc = null;
				}
			} catch (Exception e) {
				if (log.isErrorEnabled())
					log.error(e.getMessage(), e);
			}
		return doc;
	}

	@Override
	public void makeImmutable(long docId, DocumentHistory transaction) {
		Document doc = null;
		try {
			doc = findById(docId);
			initialize(doc);
			doc.setImmutable(1);
			doc.setStatus(Document.DOC_UNLOCKED);
			store(doc, transaction);
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
		}

	}

	public void deleteAll(Collection<Document> documents, DocumentHistory transaction) {
		deleteAll(documents, PersistentObject.DELETED_CODE_DEFAULT, transaction);
	}

	@Override
	public void deleteAll(Collection<Document> documents, int delCode, DocumentHistory transaction) {
		for (Document document : documents) {
			DocumentHistory deleteHistory = new DocumentHistory(transaction);
			deleteHistory.setEvent(DocumentEvent.DELETED.toString());
			delete(document.getId(), delCode, deleteHistory);
		}

	}

	public void setNoteDAO(DocumentNoteDAO noteDAO) {
		this.noteDAO = noteDAO;
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	@Override
	public void saveDocumentHistory(Document doc, DocumentHistory transaction) throws PersistenceException {
		Map<String, Object> dictionary = new HashMap<String, Object>();
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
			transaction.setFilename(doc.getFileName());
			transaction.setFileSize(doc.getFileSize());
			transaction.setNotified(0);
			transaction.setDocument(doc);
			transaction.setPath(folderDAO.computePathExtended(doc.getFolder().getId()));

			boolean saved = documentHistoryDAO.store(transaction);
			if (saved) {
				log.debug("Invoke listeners after store");
				for (DocumentListener listener : listenerManager.getListeners())
					listener.afterSaveHistory(doc, transaction, dictionary);

				EventCollector.get().newEvent(transaction);
			}
		} catch (Throwable e) {
			if (transaction != null && StringUtils.isNotEmpty(transaction.getSessionId())) {
				Session session = SessionManager.get().get(transaction.getSessionId());
				session.logError(e.getMessage());
			}
			log.error(e.getMessage(), e);
			if (e instanceof PersistenceException)
				throw (PersistenceException) e;
			else
				throw new PersistenceException(e);
		}
	}

	@Override
	public long countByIndexed(int indexed) {
		String query = "select count(*) from ld_document where ld_deleted=0 and ld_indexed = " + indexed;
		try {
			return queryForLong(query);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return 0;
		}
	}

	@Override
	public List<Long> findAliasIds(long docId) {
		try {
			return findIdsByWhere(ALIAS_ENTITY+".docRef = " + Long.toString(docId), null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@Override
	public List<Document> findDeleted(long userId, Integer maxHits) {
		List<Document> results = new ArrayList<Document>();
		try {
			String query = "select ld_id, ld_lastmodified, ld_filename, ld_customid, ld_tenantid, ld_folderid, ld_color from ld_document where ld_deleted=1 and ld_deleteuserid = "
					+ userId + " order by ld_lastmodified desc";

			@SuppressWarnings("rawtypes")
			RowMapper docMapper = new BeanPropertyRowMapper() {
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

			results = (List<Document>) query(query, null, docMapper, maxHits);

		} catch (Throwable e) {
			log.error(e.getMessage());
		}

		return results;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	@Override
	public List<Document> findByIds(Long[] ids, Integer max) {
		List<Document> docs = new ArrayList<Document>();
		if (ids.length < 1)
			return docs;

		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < ids.length; i++) {
			if (i > 0)
				sb.append(",");
			sb.append(ids[i]);
		}
		try {
			docs = findByWhere(ALIAS_ENTITY+".id in(" + sb.toString() + ")", null, max);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
		return docs;
	}

	@Override
	public boolean deleteOrphaned(long deleteUserId) {
		try {
			String dbms = config.getProperty("jdbc.dbms") != null ? config.getProperty("jdbc.dbms").toLowerCase()
					: "mysql";

			String concat = "CONCAT(ld_id,CONCAT('.',ld_customid))";
			if (dbms.contains("postgre"))
				concat = "ld_id || '.' || ld_customid";
			if (dbms.contains("mssql"))
				concat = "CAST(ld_id AS varchar) + '.' + ld_customid";

			jdbcUpdate("update ld_document set ld_deleted=1,ld_customid=" + concat + ", ld_deleteuserid=" + deleteUserId
					+ " where ld_deleted=0 and ld_folderid in (select ld_id from ld_folder where ld_deleted  > 0)");
		} catch (Exception e) {
			if (log.isErrorEnabled())
				log.error(e.getMessage(), e);
			return false;
		}

		return true;
	}

	@Override
	public Collection<Long> findPublishedIds(Collection<Long> folderIds) {
		StringBuffer query = new StringBuffer(
				"select ld_id from ld_document where ld_deleted=0 and not ld_status=" + AbstractDocument.DOC_ARCHIVED);
		if (folderIds != null && !folderIds.isEmpty()) {
			query.append(" and ld_folderid in (");
			query.append(folderIds.toString().replace('[', ' ').replace(']', ' '));
			query.append(" ) ");
		}
		query.append(" and ld_published = 1 ");
		query.append(" and ld_startpublishing <= ? ");
		query.append(" and ( ld_stoppublishing is null or ld_stoppublishing > ? )");

		Date now = new Date();

		try {
			@SuppressWarnings("unchecked")
			Collection<Long> buf = (Collection<Long>) queryForList(query.toString(), new Object[] { now, now },
					Long.class, null);
			Set<Long> ids = new HashSet<Long>();
			for (Long id : buf) {
				if (!ids.contains(id))
					ids.add(id);
			}
			return ids;
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Long>();
		}
	}

	@SuppressWarnings("unchecked")
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

	public void setTenantDAO(TenantDAO tenantDAO) {
		this.tenantDAO = tenantDAO;
	}

	@Override
	public void cleanUnexistingUniqueTags() {
		StringBuffer deleteStatement = new StringBuffer("delete from ld_uniquetag UT where ");

		// tags no more existing in the ld_tag table or that belong to deleted
		// documents
		deleteStatement.append(
				" not UT.ld_tag in (select distinct(B.ld_tag) from ld_tag B, ld_document C where UT.ld_tenantid=B.ld_tenantid and UT.ld_tag=B.ld_tag and C.ld_id=B.ld_docid and C.ld_deleted=0) ");

		// tags no more existing in the ld_foldertag table or that belong to
		// deleted folders
		deleteStatement.append(
				" and not UT.ld_tag in (select distinct(D.ld_tag) from ld_foldertag D, ld_folder E where UT.ld_tenantid=D.ld_tenantid and UT.ld_tag=D.ld_tag and E.ld_id=D.ld_folderid and E.ld_deleted=0) ");

		try {
			jdbcUpdate(deleteStatement.toString());
		} catch (PersistenceException e) {
			/*
			 * The unique SQL query failed, so we do the same deletion
			 * programmatically
			 */
			try {
				List<Long> tenantIds = tenantDAO.findAllIds();
				for (Long tenantId : tenantIds) {
					log.debug("Clean unique tags of tenant {}", tenantId);

					// Collect the currently unique used tags
					@SuppressWarnings("unchecked")
					Set<String> currentlyUsedTags = ((Map<String, String>) queryForList(
							"select distinct(B.ld_tag) from ld_tag B, ld_document C where B.ld_tenantid=" + tenantId
									+ " and C.ld_id=B.ld_docid and C.ld_deleted=0 "
									+ " UNION select distinct(D.ld_tag) from ld_foldertag D, ld_folder E where D.ld_tenantid="
									+ tenantId + " and E.ld_id=D.ld_folderid and E.ld_deleted=0",
							String.class).stream().collect(Collectors.groupingBy(Function.identity()))).keySet();

					// Delete all currently recorded unique tags no more used
					if (isOracle()) {
						/*
						 * In Oracle the limit of 1000 elements applies to sets
						 * of single items: (x) IN ((1), (2), (3), ...). There
						 * is no limit if the sets contain two or more items:
						 * (x, 0) IN ((1,0), (2,0), (3,0), ...):
						 */
						String currentlyUsedTagsStr = currentlyUsedTags.stream()
								.map(tag -> ("('" + SqlUtil.doubleQuotes(tag) + "',0)"))
								.collect(Collectors.joining(","));
						if (StringUtils.isNotEmpty(currentlyUsedTagsStr))
							jdbcUpdate("delete from ld_uniquetag where ld_tenantid=" + tenantId
									+ " and (ld_tag,0) not in (" + currentlyUsedTagsStr + ")");
					} else {
						String currentlyUsedTagsStr = currentlyUsedTags.stream()
								.map(tag -> ("'" + SqlUtil.doubleQuotes(tag) + "'")).collect(Collectors.joining(","));
						if (StringUtils.isNotEmpty(currentlyUsedTagsStr))
							jdbcUpdate("delete from ld_uniquetag where ld_tenantid=" + tenantId + " and ld_tag not in ("
									+ currentlyUsedTagsStr + ")");
					}
				}
			} catch (Throwable t) {
				log.warn(t.getMessage(), t);
			}
		}
	}

	@Override
	public void insertNewUniqueTags() {
		StringBuffer insertStatement = new StringBuffer("insert into ld_uniquetag(ld_tag, ld_tenantid, ld_count) ");
		insertStatement.append(" select distinct(B.ld_tag), B.ld_tenantid, 0 from ld_tag B, ld_document D ");
		insertStatement.append(
				" where B.ld_docid = D.ld_id and D.ld_deleted = 0 and B.ld_tag not in (select A.ld_tag from ld_uniquetag A where A.ld_tenantid=B.ld_tenantid) ");
		try {
			jdbcUpdate(insertStatement.toString());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		insertStatement = new StringBuffer("insert into ld_uniquetag(ld_tag, ld_tenantid, ld_count) ");
		insertStatement.append(" select distinct(B.ld_tag), B.ld_tenantid, 0 from ld_foldertag B, ld_folder F ");
		insertStatement.append(
				" where B.ld_folderid = F.ld_id and F.ld_deleted = 0 and B.ld_tag not in (select A.ld_tag from ld_uniquetag A where A.ld_tenantid=B.ld_tenantid) ");
		try {
			jdbcUpdate(insertStatement.toString());
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void updateCountUniqueTags() {
		List<Long> tenantIds = tenantDAO.findAllIds();
		for (Long tenantId : tenantIds) {
			// Get the actual unique tags for the given tenant
			List<String> uniqueTags = new ArrayList<String>();

			try {
				uniqueTags = (List<String>) queryForList("select ld_tag from ld_uniquetag", String.class);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			// Update the count for each tag skipping those tags that belong to
			// deleted documents
			for (String tag : uniqueTags) {
				try {
					jdbcUpdate(
							"update ld_uniquetag set ld_count = (select count(T.ld_tag) from ld_tag T, ld_document D where T.ld_tag = ? and T.ld_tenantid = ? "
									+ " and T.ld_docid = D.ld_id and D.ld_deleted=0 ) where ld_tag = ? and ld_tenantid = ?",
							tag, tenantId, tag, tenantId);
				} catch (PersistenceException e) {
					log.warn(e.getMessage(), e);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<TagCloud> getTagCloud(long tenantId, int maxTags) {
		GenericDAO gendao = (GenericDAO) Context.get().getBean(GenericDAO.class);

		List<TagCloud> list = new ArrayList<TagCloud>();
		try {
			list = (List<TagCloud>) gendao.query("select ld_tag, ld_count from ld_uniquetag where ld_tenantid="
					+ tenantId + " order by ld_count desc", null, new RowMapper<TagCloud>() {

						@Override
						public TagCloud mapRow(ResultSet rs, int arg1) throws SQLException {
							return new TagCloud(rs.getString(1), rs.getLong(2));
						}
					}, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		/**
		 * Get the most used tags
		 */
		List<TagCloud> mostUsedTags = list;
		if (maxTags > 0 && mostUsedTags.size() > maxTags)
			mostUsedTags = new ArrayList<TagCloud>(list.subList(0, maxTags));

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
	public List<TagCloud> getTagCloud(String sid) {
		Session session = SessionManager.get().get(sid);
		ContextProperties config = Context.get().getProperties();

		int maxTags = config.getInt(session.getTenantName() + ".tagcloud.maxtags", 30);
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
		assert (transaction != null);
		assert (transaction.getUsername() != null);
		transaction.setEvent(DocumentEvent.PASSWORD_PROTECTED.toString());

		Document doc = findDocument(docId);
		if (doc != null) {
			if (StringUtils.isNotEmpty(doc.getPassword()))
				throw new PersistenceException("The document already has a password, unset it first");

			initialize(doc);
			doc.setDecodedPassword(password);
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
		assert (transaction != null);
		assert (transaction.getUsername() != null);
		transaction.setEvent(DocumentEvent.PASSWORD_UNPROTECTED.toString());

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
	public Document findByPath(String path, long tenantId) {
		String folderPath = FilenameUtils.getPath(path);
		Folder folder = folderDAO.findByPathExtended(folderPath, tenantId);
		if (folder == null)
			return null;

		String fileName = FilenameUtils.getName(path);
		List<Document> docs = findByFileNameAndParentFolderId(folder.getId(), fileName, null, tenantId, null);
		for (Document doc : docs) {
			if (doc.getFileName().equals(fileName))
				return doc;
		}

		return null;
	}

	@Override
	public List<String> findDuplicatedDigests(Long tenantId, Long folderId) {
		// First of all, find all duplicates digests.
		StringBuffer digestQuery = new StringBuffer("select ld_digest from ld_document where ld_deleted = 0 ");
		if (tenantId != null) {
			digestQuery.append(" and ld_tenantid = ");
			digestQuery.append(Long.toString(tenantId));
		}

		if (folderId != null) {
			List<Long> tree = folderDAO.findIdsByParentId(folderId);
			if (!tree.contains(folderId))
				tree.add(folderId);
			digestQuery.append(" and ld_folderid in (");
			digestQuery.append(tree.stream().map(id -> id.toString()).collect(Collectors.joining(", ")));
			digestQuery.append(" ) ");
		}
		digestQuery.append(" and ld_docref is null and ld_digest is not null group by ld_digest having count(*) > 1");

		try {
			return (List<String>) query(digestQuery.toString(), null, new RowMapper<String>() {
				public String mapRow(ResultSet rs, int rowNum) throws SQLException {
					return rs.getString(1);
				}
			}, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<String>();
		}
	}
}