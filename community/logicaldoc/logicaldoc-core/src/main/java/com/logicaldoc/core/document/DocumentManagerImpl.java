package com.logicaldoc.core.document;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.pdfbox.io.MemoryUsageSetting;
import org.apache.pdfbox.multipdf.PDFMergerUtility;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MailUtil;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.ticket.Ticket;
import com.logicaldoc.core.ticket.TicketDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * Basic Implementation of <code>DocumentManager</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.5
 */
public class DocumentManagerImpl implements DocumentManager {

	protected static Logger log = LoggerFactory.getLogger(DocumentManagerImpl.class);

	private DocumentDAO documentDAO;

	private DocumentNoteDAO documentNoteDAO;

	private FolderDAO folderDAO;

	private TemplateDAO templateDAO;

	private DocumentListenerManager listenerManager;

	private VersionDAO versionDAO;

	private UserDAO userDAO;

	private TicketDAO ticketDAO;

	private SearchEngine indexer;

	private Storer storer;

	private ContextProperties config;

	public void setListenerManager(DocumentListenerManager listenerManager) {
		this.listenerManager = listenerManager;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}

	public void setTemplateDAO(TemplateDAO templateDAO) {
		this.templateDAO = templateDAO;
	}

	public void setIndexer(SearchEngine indexer) {
		this.indexer = indexer;
	}

	public void setVersionDAO(VersionDAO versionDAO) {
		this.versionDAO = versionDAO;
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	@Override
	public void replaceFile(long docId, String fileVersion, InputStream content, DocumentHistory transaction)
			throws Exception {
		assert (transaction != null);

		// Write content to temporary file, then delete it
		File tmp = File.createTempFile("replacefile", "");
		try {
			if (content != null)
				FileUtil.writeFile(content, tmp.getPath());
			replaceFile(docId, fileVersion, tmp, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public void replaceFile(long docId, String fileVersion, File newFile, DocumentHistory transaction)
			throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (transaction.getComment() != null);

		transaction.setEvent(DocumentEvent.VERSION_REPLACED.toString());
		transaction.setComment(String.format("file version %s - %s", fileVersion, transaction.getComment()));

		// identify the document and folder
		Document document = documentDAO.findDocument(docId);

		if (document.getImmutable() == 0 && document.getStatus() == Document.DOC_UNLOCKED) {
			// Remove the files of the same fileVersion
			List<String> resources = storer.listResources(document.getId(), fileVersion);
			for (String resource : resources)
				storer.delete(document.getId(), resource);

			// Store the new file
			storer.store(newFile, document.getId(), storer.getResourceName(document, fileVersion, null));

			long fileSize = newFile.length();

			// Now update the file size in the versions
			List<Version> versions = versionDAO.findByDocId(document.getId());
			for (Version version : versions) {
				if (version.getFileVersion().equals(fileVersion)) {
					versionDAO.initialize(version);
					version.setFileSize(fileSize);
					versionDAO.store(version);
				}
			}

			// Update the document's record
			documentDAO.initialize(document);
			document.setFileSize(fileSize);
			if (document.getIndexed() != Document.INDEX_SKIP)
				document.setIndexed(Document.INDEX_TO_INDEX);
			document.setOcrd(0);
			document.setBarcoded(0);
			document.setSigned(0);
			document.setStamped(0);
			documentDAO.store(document, transaction);

			log.debug("Replaced fileVersion {} of document {}", fileVersion, docId);
		}
	}

	@Override
	public void checkin(long docId, File file, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (transaction.getComment() != null);
		assert (filename != null);

		transaction.setEvent(DocumentEvent.CHECKEDIN.toString());

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			// identify the document and folder
			Document document = documentDAO.findDocument(docId);
			String oldFileVersion = document.getFileVersion();

			document.setComment(transaction.getComment());

			Document oldDocument = null;
			if (document.getImmutable() == 0) {
				documentDAO.initialize(document);

				oldDocument = (Document) document.clone();

				// Check CustomId uniqueness
				if (docVO != null && docVO.getCustomId() != null) {
					Document test = documentDAO.findByCustomId(docVO.getCustomId(), document.getTenantId());
					if (test != null && test.getId() != document.getId())
						throw new Exception("Duplicated CustomID");
				}

				/*
				 * Now apply the metadata, if any
				 */
				if (docVO != null) {
					Folder originalFolder = document.getFolder();
					String originalVersion = document.getVersion();
					String originalFileVersion = document.getFileVersion();

					document.copyAttributes(docVO);

					// Restore important original information
					document.setFolder(originalFolder);
					document.setVersion(originalVersion);
					document.setFileVersion(originalFileVersion);
				}

				try {
					checkEmailAttachments(file, document);
				} catch (Throwable t) {
					log.warn("Unable to detect the presence of attachments in the email file");
					log.debug(t.getMessage(), t);
				}

				Map<String, Object> dictionary = new HashMap<String, Object>();

				log.debug("Invoke listeners before checkin");
				for (DocumentListener listener : listenerManager.getListeners())
					listener.beforeCheckin(document, transaction, dictionary);

				document.setStamped(0);
				document.setSigned(0);
				document.setOcrd(0);
				document.setBarcoded(0);
				document.setPages(-1);

				if (document.getIndexed() != AbstractDocument.INDEX_SKIP)
					document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

				boolean stored = documentDAO.store(document);
				if (!stored)
					throw new Exception("Document not checked in");

				document = documentDAO.findById(document.getId());
				Folder folder = document.getFolder();
				documentDAO.initialize(document);

				// create some strings containing paths
				document.setFileName(filename);
				document.setType(FilenameUtils.getExtension(filename));

				// set other properties of the document
				document.setDate(new Date());
				document.setPublisher(transaction.getUsername());
				document.setPublisherId(transaction.getUserId());
				document.setStatus(Document.DOC_UNLOCKED);
				document.setLockUserId(null);
				document.setFolder(folder);
				document.setDigest(null);
				document.setFileSize(file.length());
				document.setExtResId(null);

				// Create new version (a new version number is created)
				Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
						DocumentEvent.CHECKEDIN.toString(), release);

				document.setStatus(Document.DOC_UNLOCKED);
				if (documentDAO.store(document, transaction) == false)
					throw new Exception(String.format("Errors saving document %s", document.getId()));

				// store the document in the repository (on the file system)
				try {
					storeFile(document, file);
				} catch (Throwable t) {
					log.error("Cannot save the new version {} into the storage", document, t);

					document.copyAttributes(oldDocument);
					document.setOcrd(oldDocument.getOcrd());
					document.setOcrTemplateId(oldDocument.getOcrTemplateId());
					document.setBarcoded(oldDocument.getBarcoded());
					document.setBarcodeTemplateId(oldDocument.getBarcodeTemplateId());
					document.setIndexed(oldDocument.getIndexed());
					document.setCustomId(oldDocument.getCustomId());
					document.setStatus(oldDocument.getStatus());
					document.setStamped(oldDocument.getStamped());
					document.setSigned(oldDocument.getSigned());
					documentDAO.store(document);
					throw t;
				}

				version.setFileSize(document.getFileSize());
				version.setDigest(null);
				stored = versionDAO.store(version);
				if (!stored)
					throw new Exception("Version not stored");

				log.debug("Stored version {}", version.getVersion());
				log.debug("Invoke listeners after store");
				for (DocumentListener listener : listenerManager.getListeners())
					listener.afterCheckin(document, transaction, dictionary);
				stored = documentDAO.store(document);
				if (!stored)
					throw new Exception("Document not stored");

				log.debug("Checked in document {}", docId);

				if (!document.getFileVersion().equals(oldFileVersion))
					documentNoteDAO.copyAnnotations(document.getId(), oldFileVersion, document.getFileVersion());
			}
		}
	}

	@Override
	public void checkin(long docId, InputStream content, String filename, boolean release, AbstractDocument docVO,
			DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (transaction.getComment() != null);

		// Write content to temporary file, then delete it
		File tmp = File.createTempFile("checkin", "");
		try {
			FileUtil.writeFile(content, tmp.getPath());
			checkin(docId, tmp, filename, release, docVO, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public void checkout(long docId, DocumentHistory transaction) throws Exception {
		if (transaction.getEvent() == null)
			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
		lock(docId, Document.DOC_CHECKED_OUT, transaction);
	}

	@Override
	public void lock(long docId, int status, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findDocument(docId);

			if (document.getStatus() == status && document.getLockUserId() == transaction.getUserId()) {
				log.debug("Document {} is already locked by user {}", document, transaction.getUser().getFullName());
				return;
			}

			if (document.getStatus() != Document.DOC_UNLOCKED)
				throw new Exception(String.format("Document %s is already locked by user %s and cannot be locked by %s",
						document, document.getLockUser(), transaction.getUser().getFullName()));

			documentDAO.initialize(document);
			document.setLockUserId(transaction.getUser().getId());
			document.setLockUser(transaction.getUser().getFullName());
			document.setStatus(status);
			document.setFolder(document.getFolder());

			if (transaction.getEvent() == null)
				transaction.setEvent(DocumentEvent.LOCKED.toString());

			// Modify document history entry
			boolean stored = documentDAO.store(document, transaction);
			if (!stored)
				throw new Exception("Document not locked");
		}

		log.debug("locked document {}", docId);

	}

	private void storeFile(Document doc, File file) throws IOException {
		String resource = storer.getResourceName(doc, null, null);
		storer.store(file, doc.getId(), resource);
	}

	/**
	 * Utility method for document removal from index
	 * 
	 * @param doc the document to delete from the index
	 */
	@Override
	public void deleteFromIndex(Document doc) {
		try {
			long docId = doc.getId();

			// Physically remove the document from full-text index
			if (doc != null) {
				indexer.deleteHit(docId);
			}

			doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
			documentDAO.store(doc);

			markAliasesToIndex(doc.getId());
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	/**
	 * Retrieves the document's content as a string
	 * 
	 * @param doc The document representation
	 * @return The document's content
	 */
	@Override
	public String parseDocument(Document doc, String fileVersion) {
		String content = null;

		// Check if the document is an alias
		if (doc.getDocRef() != null) {
			long docref = doc.getDocRef();
			doc = documentDAO.findById(docref);
			if (doc == null)
				throw new RuntimeException(String.format("Unexisting referenced document {}", docref));
		}

		// Parses the file where it is already stored
		Locale locale = doc.getLocale();
		String resource = storer.getResourceName(doc, fileVersion, null);
		Parser parser = ParserFactory.getParser(doc.getFileName());

		log.debug("Using parser {} to parse document {}", parser.getClass().getName(), doc.getId());

		// and gets some fields
		if (parser != null) {
			TenantDAO tDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
			try {
				content = parser.parse(storer.getStream(doc.getId(), resource), doc.getFileName(), null, locale,
						tDao.findById(doc.getTenantId()).getName());
			} catch (IOException e) {
				log.error("Cannot retrieve content of document {}", doc, e);
			}
		}

		if (content == null) {
			content = "";
		}

		return content;
	}

	@Override
	public long reindex(long docId, String content) throws Exception {
		Document doc = documentDAO.findById(docId);
		if (doc == null) {
			log.warn("Unexisting document with ID: {}", docId);
			return 0;
		}

		log.debug("Reindexing document {} - {}", docId, doc.getFileName());

		try {
			boolean alreadyIndexed = doc.getIndexed() == AbstractDocument.INDEX_INDEXED;

			long parsingTime = 0;
			String cont = content;
			if (StringUtils.isEmpty(cont) && doc.getIndexed() != AbstractDocument.INDEX_TO_INDEX_METADATA) {
				// Extracts the content from the file. This may take very long
				// time.
				Date beforeParsing = new Date();
				cont = parseDocument(doc, null);
				parsingTime = TimeDiff.getTimeDifference(beforeParsing, new Date(), TimeField.MILLISECOND);
			}

			// This may take time
			indexer.addHit(doc, cont);

			// For additional safety update the DB directly
			doc.setIndexed(AbstractDocument.INDEX_INDEXED);
			boolean stored = documentDAO.store(doc);
			if (!stored)
				throw new Exception("Document not stored");

			/*
			 * If the document was already indexed, mark the aliases to be
			 * re-indexed
			 */
			if (alreadyIndexed)
				markAliasesToIndex(docId);

			return parsingTime;
		} catch (Throwable e) {
			log.error("Error reindexing document {} - {}", docId, doc.getFileName());
			throw e;
		}
	}

	private void markAliasesToIndex(long referencedDocId) throws PersistenceException {
		documentDAO.jdbcUpdate("update ld_document set ld_indexed=" + AbstractDocument.INDEX_TO_INDEX
				+ " where ld_docref=" + referencedDocId + " and not ld_id = " + referencedDocId);
	}

	@Override
	public void update(Document doc, Document docVO, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);
		assert (doc != null);
		assert (docVO != null);
		try {

			/*
			 * Better to synchronize this block because under high
			 * multi-threading may lead to hibernate's sessions rollbacks
			 */
			synchronized (this) {
				documentDAO.initialize(doc);
				if (doc.getImmutable() == 0
						|| ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {
					DocumentHistory renameTransaction = null;
					if (!doc.getFileName().equals(docVO.getFileName()) && docVO.getFileName() != null) {
						renameTransaction = (DocumentHistory) transaction.clone();
						renameTransaction.setFilenameOld(doc.getFileName());
						renameTransaction.setEvent(DocumentEvent.RENAMED.toString());
					}

					// Check CustomId uniqueness
					if (docVO.getCustomId() != null) {
						Document test = documentDAO.findByCustomId(docVO.getCustomId(), docVO.getTenantId());
						if (test != null && test.getId() != doc.getId())
							throw new Exception("Duplicated CustomID");
						doc.setCustomId(docVO.getCustomId());
					}

					// The document must be re-indexed
					doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);

					doc.setWorkflowStatus(docVO.getWorkflowStatus());

					// Save retention policies
					doc.setPublished(docVO.getPublished());
					doc.setStartPublishing(docVO.getStartPublishing());
					doc.setStopPublishing(docVO.getStopPublishing());

					// Intercept locale changes
					if (!doc.getLocale().equals(docVO.getLocale())) {
						indexer.deleteHit(doc.getId());
						doc.setLocale(docVO.getLocale());
					}

					if (StringUtils.isNotEmpty(docVO.getFileName())) {
						if (!doc.getFileName().equals(docVO.getFileName())) {
							doc.setFileName(docVO.getFileName());
						}
					}

					doc.clearTags();
					doc.setTags(docVO.getTags());

					Template template = docVO.getTemplate();
					if (template == null && docVO.getTemplateId() != null)
						template = templateDAO.findById(docVO.getTemplateId());

					// Change the template and attributes
					if (template != null) {
						doc.setTemplate(template);
						doc.setTemplateId(template.getId());
						if (docVO.getAttributes() != null) {
							doc.getAttributes().clear();
							for (String attrName : docVO.getAttributes().keySet()) {
								Attribute docExtendedAttribute = docVO.getAttributes().get(attrName);
								doc.getAttributes().put(attrName, docExtendedAttribute);
							}
						}
					} else {
						doc.setTemplate(null);
					}

					if (doc.getTemplate() == null) {
						doc.setOcrTemplateId(null);
						doc.setOcrd(0);
					}

					if ((doc.getOcrTemplateId() == null && docVO.getOcrTemplateId() != null)
							|| (doc.getOcrTemplateId() != null && docVO.getOcrTemplateId() == null)
							|| (doc.getOcrTemplateId() == null && docVO.getOcrTemplateId() == null)
							|| !doc.getOcrTemplateId().equals(docVO.getOcrTemplateId()))
						doc.setOcrd(0);
					else
						doc.setOcrd(docVO.getOcrd());
					doc.setOcrTemplateId(docVO.getOcrTemplateId());

					if ((doc.getBarcodeTemplateId() == null && docVO.getBarcodeTemplateId() != null)
							|| (doc.getBarcodeTemplateId() != null && docVO.getBarcodeTemplateId() == null)
							|| (doc.getBarcodeTemplateId() == null && docVO.getBarcodeTemplateId() == null)
							|| !doc.getBarcodeTemplateId().equals(docVO.getBarcodeTemplateId()))
						doc.setBarcoded(0);
					else
						doc.setBarcoded(docVO.getBarcoded());
					doc.setBarcodeTemplateId(docVO.getBarcodeTemplateId());

					// create a new version
					Version version = Version.create(doc, transaction.getUser(), transaction.getComment(),
							DocumentEvent.CHANGED.toString(), false);

					boolean stored = false;

					// Modify document history entry
					doc.setVersion(version.getVersion());
					if (renameTransaction != null) {
						renameTransaction.setUser(transaction.getUser());
						stored = documentDAO.store(doc, renameTransaction);
					} else {
						stored = documentDAO.store(doc, transaction);
					}

					if (!stored)
						throw new Exception("Document not stored");

					stored = versionDAO.store(version);
					if (!stored)
						throw new Exception("Version not stored");

					markAliasesToIndex(doc.getId());
				} else {
					throw new Exception(String.format("Document %s is immutable", doc));
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public void moveToFolder(Document doc, Folder folder, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		if (folder.equals(doc.getFolder()))
			return;

		if (doc.getImmutable() == 0 || ((doc.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {

			/*
			 * Better to synchronize this block because under high
			 * multi-threading may lead to hibernate's sessions rollbacks
			 */
			synchronized (this) {
				documentDAO.initialize(doc);
				transaction.setPathOld(folderDAO.computePathExtended(doc.getFolder().getId()));
				transaction.setFilenameOld(doc.getFileName());
				transaction.setEvent(DocumentEvent.MOVED.toString());

				doc.setFolder(folder);

				// The document needs to be reindexed
				if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED) {
					doc.setIndexed(AbstractDocument.INDEX_TO_INDEX);
					indexer.deleteHit(doc.getId());

					// The same thing should be done on each shortcut
					documentDAO.jdbcUpdate("update ld_document set ld_indexed=" + AbstractDocument.INDEX_TO_INDEX
							+ " where ld_docref=" + doc.getId());
				}

				// Modify document history entry
				if (transaction.getEvent().trim().isEmpty())
					transaction.setEvent(DocumentEvent.MOVED.toString());

				Version version = Version.create(doc, transaction.getUser(), transaction.getComment(),
						DocumentEvent.MOVED.toString(), false);
				version.setId(0);

				boolean stored = documentDAO.store(doc, transaction);
				if (!stored)
					throw new Exception("Document not stored");
				stored = versionDAO.store(version);
				if (!stored)
					throw new Exception("Version not stored");
			}
		} else {
			throw new Exception("Document is immutable");
		}
	}

	@Override
	public Document create(InputStream content, Document docVO, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (docVO != null);

		// Write content to temporary file, then delete it
		File tmp = File.createTempFile("create", "");
		try {
			if (content != null)
				FileUtil.writeFile(content, tmp.getPath());
			return create(tmp, docVO, transaction);
		} finally {
			FileUtils.deleteQuietly(tmp);
		}
	}

	@Override
	public Document create(File file, Document docVO, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (docVO != null);

		boolean stored = false;
		try {
			String type = "unknown";
			int lastDotIndex = docVO.getFileName().lastIndexOf(".");
			if (lastDotIndex > 0) {
				type = FilenameUtils.getExtension(docVO.getFileName()).toLowerCase();
			}

			if (docVO.getDate() == null)
				docVO.setDate(new Date());

			if (docVO.getCreation() == null)
				docVO.setCreation(docVO.getDate());

			if (StringUtils.isNotEmpty(docVO.getPublisher()))
				docVO.setPublisher(docVO.getPublisher());
			else
				docVO.setPublisher(transaction.getUsername());

			if (docVO.getPublisherId() != 0L)
				docVO.setPublisherId(docVO.getPublisherId());
			else
				docVO.setPublisherId(transaction.getUserId());

			if (StringUtils.isNotEmpty(docVO.getCreator()))
				docVO.setCreator(docVO.getCreator());
			else
				docVO.setCreator(transaction.getUsername());

			if (docVO.getCreatorId() != 0L)
				docVO.setCreatorId(docVO.getCreatorId());
			else
				docVO.setCreatorId(transaction.getUserId());

			docVO.setStatus(Document.DOC_UNLOCKED);
			docVO.setType(type);
			docVO.setVersion(config.getProperty("document.startversion"));
			docVO.setFileVersion(docVO.getVersion());
			docVO.setFileSize(file.length());
			docVO.setId(0L);

			/*
			 * Better to synchronize this block because under high
			 * multi-threading may lead to hibernate's sessions rollbacks
			 */
			synchronized (this) {
				if (docVO.getTemplate() == null && docVO.getTemplateId() != null)
					docVO.setTemplate(templateDAO.findById(docVO.getTemplateId()));

				if (file != null)
					transaction.setFile(file.getAbsolutePath());

				try {
					checkEmailAttachments(file, docVO);
				} catch (Throwable t) {
					log.warn("Unable to detect the presence of attachments in the email file");
					log.debug(t.getMessage(), t);
				}

				// Create the record
				transaction.setEvent(DocumentEvent.STORED.toString());
				stored = documentDAO.store(docVO, transaction);

				if (stored) {
					/* store the document into filesystem */
					if (file != null)
						try {
							storeFile(docVO, file);
						} catch (Throwable e) {
							String message = String.format("Unable to store the file of document %d", docVO.getId());
							log.error(message);
							documentDAO.delete(docVO.getId());
							throw new Exception(message, e);
						}

					// Store the initial version (default 1.0)
					Version vers = Version.create(docVO, userDAO.findById(transaction.getUserId()),
							transaction.getComment(), DocumentEvent.STORED.toString(), true);
					versionDAO.store(vers);
					log.debug("Stored version {}", vers.getVersion());
					return docVO;
				} else
					throw new Exception("Document not stored");
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new Exception(e);
		}
	}

	/**
	 * In case of email, put pages to 2 to mark the existence of attachments
	 * 
	 * @throws Exception
	 */
	private void checkEmailAttachments(File file, Document doc) throws Exception {
		if (doc.getFileName().toLowerCase().endsWith(".eml") || doc.getFileName().toLowerCase().endsWith(".msg")) {
			boolean attachments = doc.getFileName().endsWith(".eml") ? MailUtil.emlContainsAttachments(file)
					: MailUtil.msgContainsAttachments(file);
			if (attachments)
				doc.setPages(2);

			if (doc.getTemplate() != null && doc.getTemplate().getName().equals("email")
					&& doc.getValue("attachments") != null && "true".equals(doc.getValue("attachments")))
				doc.setPages(2);
		}
	}

	public Document copyToFolder(Document doc, Folder folder, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		// initialize the document
		documentDAO.initialize(doc);

		if (doc.getDocRef() != null) {
			return createAlias(doc, folder, doc.getDocRefType(), transaction);
		}

		String resource = storer.getResourceName(doc, null, null);
		InputStream is = storer.getStream(doc.getId(), resource);
		try {
			Document cloned = (Document) doc.clone();
			cloned.setId(0);
			if (doc.getFolder().getId() != folder.getId())
				cloned.setFolder(folder);
			cloned.setLastModified(null);
			cloned.setDate(null);
			if (cloned.getIndexed() == Document.INDEX_INDEXED)
				cloned.setIndexed(Document.INDEX_TO_INDEX);
			cloned.setStamped(0);
			cloned.setSigned(0);
			cloned.setLinks(0);
			cloned.setOcrd(0);
			cloned.setBarcoded(0);
			Document createdDocument = create(is, cloned, transaction);

			// Save the event of the copy
			DocumentHistory copyEvent = (DocumentHistory) transaction.clone();
			copyEvent.setDocument(doc);
			copyEvent.setFolder(doc.getFolder());
			copyEvent.setEvent(DocumentEvent.COPYED.toString());

			String newPath = folderDAO.computePathExtended(folder.getId());
			copyEvent.setComment(newPath + "/" + createdDocument.getFileName());
			documentDAO.saveDocumentHistory(doc, copyEvent);

			return createdDocument;
		} finally {
			if (is != null)
				is.close();
			is = null;
		}
	}

	@Override
	public void unlock(long docId, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUserId() != null);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findDocument(docId);
			documentDAO.initialize(document);

			if (transaction.getUser().isMemberOf("admin")) {
				document.setImmutable(0);
			} else if (document.getLockUserId() == null || document.getStatus() == Document.DOC_UNLOCKED) {
				log.debug("The document {} is already unlocked", document);
				return;
			} else if (!transaction.getUserId().toString().equals(document.getLockUserId().toString())) {
				/*
				 * We compare the string representations because found that
				 * sometimes the comparison between longs fails
				 */
				String message = String.format("The document %s is locked by %s and cannot be unlocked by %s", document,
						document.getLockUser(), transaction.getUser().getFullName());
				throw new Exception(message);
			}

			document.setLockUserId(null);
			document.setLockUser(null);
			document.setExtResId(null);
			document.setStatus(Document.DOC_UNLOCKED);

			// Modify document history entry
			transaction.setEvent(DocumentEvent.UNLOCKED.toString());
			boolean stored = documentDAO.store(document, transaction);
			if (!stored)
				throw new Exception("Document not unlocked");
		}
		log.debug("Unlocked document {}", docId);
	}

	@Override
	public void makeImmutable(long docId, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);
		if (document.getImmutable() == 0) {
			// Modify document history entry
			transaction.setEvent(DocumentEvent.IMMUTABLE.toString());
			documentDAO.makeImmutable(docId, transaction);

			log.debug("The document {} has been marked as immutable", docId);
		} else {
			throw new Exception("Document is immutable");
		}
	}

	@Override
	public void rename(long docId, String newName, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		/*
		 * Better to synchronize this block because under high multi-threading
		 * may lead to hibernate's sessions rollbacks
		 */
		synchronized (this) {
			Document document = documentDAO.findDocument(docId);

			if (document.getImmutable() == 0
					|| ((document.getImmutable() == 1 && transaction.getUser().isMemberOf("admin")))) {
				documentDAO.initialize(document);
				document.setFileName(newName.trim());
				String extension = FilenameUtils.getExtension(newName.trim());
				if (StringUtils.isNotEmpty(extension)) {
					document.setType(FilenameUtils.getExtension(newName));
				} else {
					document.setType("unknown");
				}

				document.setIndexed(AbstractDocument.INDEX_TO_INDEX);

				Version version = Version.create(document, transaction.getUser(), transaction.getComment(),
						DocumentEvent.RENAMED.toString(), false);
				versionDAO.store(version);

				transaction.setEvent(DocumentEvent.RENAMED.toString());
				if (documentDAO.store(document, transaction) == false)
					throw new Exception(String.format("Errors saving document %s", document.getId()));

				markAliasesToIndex(docId);
				log.debug("Document renamed: {}", document.getId());
			} else {
				throw new Exception("Document is immutable");
			}
		}
	}

	@Override
	public Document replaceAlias(long aliasId, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		try {
			// get the alias
			Document alias = documentDAO.findById(aliasId);
			if (alias == null || alias.getDocRef() == null)
				throw new Exception(String.format("Unable to find alias %s", aliasId));

			Folder folder = alias.getFolder();
			folderDAO.initialize(folder);

			if (!folderDAO.isWriteEnabled(alias.getFolder().getId(), transaction.getUserId()))
				throw new Exception(String.format("User %s without WRITE permission in folder %s",
						transaction.getUsername(), folder.getId()));

			Document originalDoc = documentDAO.findById(alias.getDocRef());
			documentDAO.initialize(originalDoc);
			documentDAO.delete(aliasId, transaction);

			return copyToFolder(originalDoc, folder, (DocumentHistory) transaction.clone());
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public Document createAlias(Document doc, Folder folder, String aliasType, DocumentHistory transaction)
			throws Exception {
		assert (doc != null);
		assert (folder != null);
		assert (transaction != null);
		assert (transaction.getUser() != null);

		try {
			// initialize the document
			documentDAO.initialize(doc);

			Document alias = new Document();
			alias.setFolder(folder);
			alias.setFileName(doc.getFileName());
			alias.setFileSize(doc.getFileSize());
			alias.setDate(new Date());
			alias.setVersion(doc.getVersion());
			alias.setFileVersion(doc.getFileVersion());

			String type = "unknown";
			int lastDotIndex = doc.getFileName().lastIndexOf(".");
			if (lastDotIndex > 0)
				type = FilenameUtils.getExtension(doc.getFileName());

			if (StringUtils.isNotEmpty(aliasType)) {
				alias.setFileName(FilenameUtils.getBaseName(doc.getFileName()) + "."
						+ FilenameUtils.getExtension(aliasType).toLowerCase());
				type = FilenameUtils.getExtension(aliasType).toLowerCase();
			}

			alias.setPublisher(transaction.getUsername());
			alias.setPublisherId(transaction.getUserId());
			alias.setCreator(transaction.getUsername());
			alias.setCreatorId(transaction.getUserId());
			alias.setStatus(Document.DOC_UNLOCKED);
			alias.setType(type);

			// Set the Doc Reference
			if (doc.getDocRef() == null) {
				// Set the docref as the id of the original document
				alias.setDocRef(doc.getId());
			} else {
				// The doc is a shortcut, so we still copy a shortcut
				alias.setDocRef(doc.getDocRef());
			}
			alias.setDocRefType(aliasType);

			// Modify document history entry
			transaction.setEvent(DocumentEvent.SHORTCUT_STORED.toString());

			documentDAO.store(alias, transaction);

			return alias;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public void changeIndexingStatus(Document doc, int status) {
		assert (status != AbstractDocument.INDEX_INDEXED);

		if (status == AbstractDocument.INDEX_SKIP && doc.getIndexed() == AbstractDocument.INDEX_SKIP)
			return;
		if (status == AbstractDocument.INDEX_TO_INDEX && doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX)
			return;
		if (status == AbstractDocument.INDEX_TO_INDEX_METADATA
				&& doc.getIndexed() == AbstractDocument.INDEX_TO_INDEX_METADATA)
			return;

		documentDAO.initialize(doc);
		if (doc.getIndexed() == AbstractDocument.INDEX_INDEXED)
			deleteFromIndex(doc);
		doc.setIndexed(status);
		try {
			documentDAO.store(doc);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	public void setUserDAO(UserDAO userDAO) {
		this.userDAO = userDAO;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	@Override
	public Version deleteVersion(long versionId, DocumentHistory transaction) throws Exception {
		Version versionToDelete = versionDAO.findById(versionId);
		assert (versionToDelete != null);

		String versionToDeleteSpec = versionToDelete.getVersion();

		Document document = documentDAO.findById(versionToDelete.getDocId());
		assert (document != null);

		List<Version> versions = versionDAO.findByDocId(versionToDelete.getDocId());

		// Exit if there is only one version
		if (versions.size() == 1)
			return versions.get(0);

		// Iterate over the versions to check if the file is referenced by other
		// versions
		boolean referenced = false;
		for (Version v : versions)
			if (v.getId() != versionId && versionToDelete.getFileVersion().equals(v.getFileVersion())) {
				referenced = true;
				break;
			}

		// If no more referenced, can delete the document's resources
		if (!referenced) {
			List<String> resources = storer.listResources(versionToDelete.getDocId(), versionToDelete.getFileVersion());
			for (String resource : resources)
				try {
					storer.delete(versionToDelete.getDocId(), resource);
				} catch (Throwable t) {
					log.warn("Unable to delete resource {} of document {}", resource, versionToDelete.getDocId());
				}
		}

		boolean deleted = versionDAO.delete(versionId);
		if (!deleted)
			throw new Exception("Version not deleted from the database");

		// Save the version deletion history
		DocumentHistory delHistory = null;
		if (transaction != null) {
			delHistory = (DocumentHistory) transaction.clone();
			delHistory.setEvent(DocumentEvent.VERSION_DELETED.toString());
			delHistory.setComment(versionToDeleteSpec + " - " + versionToDelete.getFileVersion());
		}
		documentDAO.saveDocumentHistory(document, delHistory);

		versions = versionDAO.findByDocId(versionToDelete.getDocId());

		Version lastVersion = null;
		for (Version version : versions) {
			if (version.getDeleted() == 0 && version.getId() != versionToDelete.getId()) {
				lastVersion = version;
				break;
			}
		}

		/*
		 * Downgrade the document version in case the deleted version is the
		 * actual one
		 */
		String currentVersion = document.getVersion();
		if (currentVersion.equals(versionToDeleteSpec)) {
			documentDAO.initialize(document);
			document.setVersion(lastVersion.getVersion());
			document.setFileVersion(lastVersion.getFileVersion());

			if (transaction != null) {
				transaction.setEvent(DocumentEvent.CHANGED.toString());
				transaction.setComment(
						"Version changed to " + document.getVersion() + " (" + document.getFileVersion() + ")");
			}

			boolean stored = documentDAO.store(document, transaction);
			if (!stored)
				throw new Exception("Document not stored");
		}

		return lastVersion;
	}

	public void setDocumentNoteDAO(DocumentNoteDAO documentNoteDAO) {
		this.documentNoteDAO = documentNoteDAO;
	}

	@Override
	public long archiveFolder(long folderId, DocumentHistory transaction) throws Exception {
		List<Long> docIds = new ArrayList<Long>();
		Folder root = folderDAO.findFolder(folderId);

		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, root.getId(), true);
		for (Long fid : folderIds) {
			String where = " where ld_deleted=0 and not ld_status=" + AbstractDocument.DOC_ARCHIVED
					+ " and ld_folderid=" + fid;
			@SuppressWarnings("unchecked")
			List<Long> ids = (List<Long>) documentDAO.queryForList("select ld_id from ld_document " + where,
					Long.class);
			if (ids.isEmpty())
				continue;
			docIds.addAll(ids);
			long[] idsArray = new long[ids.size()];
			for (int i = 0; i < idsArray.length; i++)
				idsArray[i] = ids.get(i).longValue();
			archiveDocuments(idsArray, transaction);
		}

		return docIds.size();
	}

	@Override
	public void archiveDocuments(long[] docIds, DocumentHistory transaction) throws Exception {
		assert (transaction.getUser() != null);
		List<Long> idsList = new ArrayList<Long>();
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		Collection<Long> folderIds = folderDAO.findFolderIdByUserIdAndPermission(transaction.getUserId(),
				Permission.ARCHIVE, null, true);

		for (long id : docIds) {
			Document doc = dao.findById(id);

			// Skip documents in folders without Archive permission
			if (!(transaction.getUser().isMemberOf("admin") || transaction.getUser().getUsername().equals("_retention"))
					&& !folderIds.contains(doc.getFolder().getId()))
				continue;

			// Create the document history event
			DocumentHistory t = (DocumentHistory) transaction.clone();
			if (dao.archive(id, t))
				idsList.add(id);
		}

		// Remove all corresponding hits from the index
		SearchEngine engine = (SearchEngine) Context.get().getBean(SearchEngine.class);
		engine.deleteHits(idsList);

		log.info("Archived documents {}", idsList);
	}

	@Override
	public Ticket createDownloadTicket(long docId, String suffix, Integer expireHours, Date expireDate,
			Integer maxDownloads, String urlPrefix, DocumentHistory transaction) throws Exception {
		assert (transaction.getUser() != null);

		Document document = documentDAO.findById(docId);
		if (document == null)
			throw new Exception("Unexisting document");

		if (!folderDAO.isDownloadEnabled(document.getFolder().getId(), transaction.getUserId()))
			throw new RuntimeException("You don't have the download permission");

		Ticket ticket = prepareTicket(docId, transaction.getUser());
		ticket.setSuffix(suffix);
		ticket.setEnabled(1);
		ticket.setMaxCount(maxDownloads);

		Calendar cal = GregorianCalendar.getInstance();
		if (expireDate != null) {
			cal.setTime(expireDate);
			cal.set(Calendar.HOUR_OF_DAY, 23);
			cal.set(Calendar.MINUTE, 59);
			cal.set(Calendar.SECOND, 59);
			cal.set(Calendar.MILLISECOND, 999);
			ticket.setExpired(cal.getTime());
		} else if (expireHours != null) {
			cal.add(Calendar.HOUR_OF_DAY, expireHours.intValue());
			ticket.setExpired(cal.getTime());
		} else {
			cal.add(Calendar.HOUR_OF_DAY, config.getInt("ticket.ttl"));
			ticket.setExpired(cal.getTime());
		}

		transaction.setEvent(DocumentEvent.DTICKET_CREATED.toString());
		transaction.setSessionId(transaction.getSessionId());

		ticketDAO.store(ticket, transaction);

		// Try to clean the DB from old tickets
		ticketDAO.deleteExpired();

		ticket.setUrl(composeTicketUrl(ticket, urlPrefix));

		return ticket;
	}

	private Ticket prepareTicket(long docId, User user) {
		Ticket ticket = new Ticket();
		ticket.setDocId(docId);
		ticket.setUserId(user.getId());
		return ticket;
	}

	private String composeTicketUrl(Ticket ticket, String urlPrefix) {
		if (StringUtils.isEmpty(urlPrefix))
			urlPrefix = config.getProperty("server.url");
		if (!urlPrefix.endsWith("/"))
			urlPrefix += "/";
		String address = urlPrefix + "download-ticket?ticketId=" + ticket.getTicketId();
		return address;
	}

	public void setTicketDAO(TicketDAO ticketDAO) {
		this.ticketDAO = ticketDAO;
	}

	@Override
	public boolean unprotect(String sid, long docId, String password) {
		Session session = SessionManager.get().get(sid);
		if (!session.isOpen())
			return false;

		if (session.getUnprotectedDocs().containsKey(docId))
			return session.getUnprotectedDocs().get(docId).equals(password);

		Document doc = documentDAO.findDocument(docId);
		if (!doc.isPasswordProtected())
			return true;

		boolean granted = doc.isGranted(password);
		if (granted)
			session.getUnprotectedDocs().put(docId, password);
		return granted;
	}

	@Override
	public void promoteVersion(long docId, String version, DocumentHistory transaction) throws Exception {
		assert (transaction != null);
		assert (transaction.getUser() != null);

		transaction.setComment(String.format("promoted version %s", version));

		// identify the document and folder
		Document document = documentDAO.findDocument(docId);
		if (document.getImmutable() == 0 && document.getStatus() == Document.DOC_UNLOCKED) {
			Version ver = versionDAO.findByVersion(document.getId(), version);
			assert (ver != null);
			versionDAO.initialize(ver);

			transaction.setEvent(DocumentEvent.CHECKEDOUT.toString());
			checkout(document.getId(), transaction);

			// Write the version file into a temporary file
			File tmp = File.createTempFile("promotion", "");
			try {
				Folder originalFolder = document.getFolder();
				Version docVO = (Version) ver.clone();
				docVO.setFolder(originalFolder);
				docVO.setCustomId(null);
				docVO.setId(0L);

				if (ver.getTemplateId() != null)
					docVO.setTemplate(templateDAO.findById(ver.getTemplateId()));

				if (StringUtils.isNotEmpty(ver.getTgs())) {
					Set<String> tags = Arrays.asList(ver.getTgs().split(",")).stream().collect(Collectors.toSet());
					docVO.setTagsFromWords(tags);
				}

				storer.writeToFile(document.getId(), storer.getResourceName(document, ver.getFileVersion(), null), tmp);
				DocumentHistory checkinTransaction = (DocumentHistory) transaction.clone();
				checkinTransaction.setDate(new Date());
				checkin(document.getId(), tmp, ver.getFileName(), false, docVO, checkinTransaction);

				log.debug("Promoted version {} of document {}", version, docId);
			} finally {
				FileUtils.deleteQuietly(tmp);
			}
		}
	}

	@Override
	public int enforceFilesIntoFolderStorage(long rootFolderId, DocumentHistory transaction) throws Exception {
		Folder rootFolder = folderDAO.findFolder(rootFolderId);
		if (rootFolder == null)
			throw new Exception("Unexisting folder ID  " + rootFolderId);

		if (transaction != null)
			transaction.setEvent(DocumentEvent.CHANGED.toString());

		int totalMovedFiles = 0;

		// Traverse the tree
		Collection<Long> folderIds = folderDAO.findFolderIdInTree(rootFolderId, false);
		for (Long folderId : folderIds) {
			Folder folder = folderDAO.findById(folderId);
			if (folder == null || folder.getFoldRef() != null)
				continue;

			// Retrieve the storage specification from the current folder
			int targetStorage = config.getInt("store.write", 1);
			if (folder.getStorage() != null)
				targetStorage = folder.getStorage().intValue();
			else {
				// Check if one of the parent folders references the storer
				List<Folder> parents = folderDAO.findParents(folderId);
				Collections.reverse(parents);

				for (Folder parentFolder : parents)
					if (parentFolder.getStorage() != null) {
						targetStorage = parentFolder.getStorage().intValue();
						break;
					}
			}

			log.info("Move the files of all the documents inside the folder {} into the target storage {}", rootFolder,
					targetStorage);

			List<Document> documents = documentDAO.findByFolder(folderId, null);
			for (Document document : documents) {
				int movedFiles = storer.moveResourcesToStore(document.getId(), targetStorage);
				if (movedFiles > 0) {
					totalMovedFiles += movedFiles;
					try {
						DocumentHistory storedTransaction = transaction.clone();
						storedTransaction
								.setComment(String.format("%d files moved to storage %d", movedFiles, targetStorage));
						documentDAO.saveDocumentHistory(document, transaction);
					} catch (Throwable t) {
						log.warn("Cannot record history for document {}", document, t);
					}
				}
			}
		}

		return totalMovedFiles;
	}

	@Override
	public Document merge(Collection<Document> documents, long targetFolderId, String fileName,
			DocumentHistory transaction) throws Exception {
		List<Long> docIds = documents.stream().map(d -> d.getId()).collect(Collectors.toList());
		File tempDir = null;
		File bigPdf = null;
		try {
			tempDir = preparePdfs(transaction != null ? transaction.getUser() : null, docIds);

			// Now collect and sort each PDF
			File[] pdfs = tempDir.listFiles();

			Arrays.sort(pdfs, new Comparator<File>() {
				@Override
				public int compare(File o1, File o2) {
					return o1.getName().compareTo(o2.getName());
				}
			});

			// Merge all the PDFs
			bigPdf = mergePdf(pdfs);

			// Add an history entry to track the export of the document
			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			for (Long id : docIds) {
				DocumentHistory trans = transaction.clone();
				trans.setEvent(DocumentEvent.EXPORTPDF.toString());
				docDao.saveDocumentHistory(docDao.findById(id), trans);
			}

			Document docVO = new Document();
			docVO.setFileName(fileName.toLowerCase().endsWith(".pdf") ? fileName : fileName + ".pdf");
			FolderDAO folderDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			docVO.setFolder(folderDao.findById(targetFolderId));

			DocumentManager manager = (DocumentManager) Context.get().getBean(DocumentManager.class);
			return manager.create(bigPdf, docVO, transaction);
		} finally {
			try {
				FileUtil.strongDelete(bigPdf);
			} catch (Throwable r) {
			}
			try {
				FileUtil.strongDelete(tempDir);
			} catch (Throwable r) {
			}
		}
	}

	/**
	 * Convert a selection of documents into PDF and stores them in a temporary
	 * folder
	 * 
	 * @param user The current user
	 * @param docIds List of documents to be converted
	 * 
	 * @return The temporary folder
	 * 
	 * @throws IOException
	 */
	private File preparePdfs(User user, List<Long> docIds) throws IOException {
		File temp = File.createTempFile("merge", "");
		temp.delete();
		temp.mkdir();

		DecimalFormat nf = new DecimalFormat("00000000");
		int i = 0;
		for (long docId : docIds) {
			try {
				i++;
				DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
				Document document = docDao.findDocument(docId);

				if (document != null && user != null && !user.isMemberOf("admin") && !user.isMemberOf("publisher")
						&& !document.isPublishing())
					continue;

				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
				manager.convertToPdf(document, null);

				File pdf = new File(temp, nf.format(i) + ".pdf");

				manager.writePdfToFile(document, null, pdf, null);
			} catch (Throwable t) {
				log.error(t.getMessage(), t);
			}
		}
		return temp;
	}

	/**
	 * Merges different PDFs into a single PDF-
	 * 
	 * @param pdfs ordered array of pdf files to be merged
	 * @return The merged Pdf file
	 * 
	 * @throws IOException
	 * @throws COSVisitorException
	 */
	private static File mergePdf(File[] pdfs) throws IOException {

		File temp = null;
		try {
			temp = File.createTempFile("merge", "");
			temp.delete();
			temp.mkdir();

			File dst = null;
			dst = File.createTempFile("merge", ".pdf");

			PDFMergerUtility merger = new PDFMergerUtility();
			for (File file : pdfs) {
				merger.addSource(file);
			}

			merger.setDestinationFileName(dst.getAbsolutePath());
			MemoryUsageSetting memoryUsage = MemoryUsageSetting.setupTempFileOnly();
			memoryUsage.setTempDir(temp);
			merger.mergeDocuments(memoryUsage);

			return dst;
		} finally {
			if (temp != null && temp.exists())
				FileUtil.strongDelete(temp);
		}
	}
}